module UserInput exposing (..)

import Dict exposing (update)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import List exposing (tail)
import Parser exposing ((|.), (|=), Parser)
import SharedStructures as Shared exposing (..)
import SimplyTypedLambdaCalculus as STLC exposing (createRuleTree, getContextFromRuleTree, getSelectedRuleTreeNode, showContext, showTerm, showVar)


newInput : String -> String -> (String -> Msg) -> Html Msg
newInput val id toMsg =
    input
        [ type_ "text"
        , value val
        , for id
        , onInput toMsg
        , onBlur TransformInput
        , Html.Events.stopPropagationOn "keydown" (Decode.succeed ( NoOperation, True )) -- Is required to stop unwanted key events from happening when typing in the text inputs.
        ]
        []


newButton : String -> msg -> Html msg
newButton txt msg =
    button
        [ classList []
        , onClick msg
        ]
        [ text txt ]


inputBlock : (Model -> Html Msg) -> Model -> Msg -> Html Msg
inputBlock inputType model msg =
    div [ class "menu__meta-variable-input-block" ]
        [ inputType model
        , button [ onClick msg ] [ text "💡" ]
        ]


fillGammaInputFromRuleTree : RuleTree -> Model -> Model
fillGammaInputFromRuleTree ruleTree model =
    let
        contextAsString =
            getContextFromRuleTree ruleTree |> showContext
    in
    { model | gammaInput = contextAsString }


fillXInputFromRuleTree : RuleTree -> Model -> Model
fillXInputFromRuleTree ruleTree model =
    case ruleTree of
        RVar _ (Var var) _ _ ->
            { model | xInput = showVar var }

        RAbs _ (Abs var _) _ _ ->
            { model | xInput = showVar var }

        _ ->
            model


fillMInputFromRuleTree : RuleTree -> Model -> Model
fillMInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ (App mTerm _) _ _ _ ->
            { model | mInput = showTerm mTerm }

        RAbs _ (Abs _ mTerm) _ _ ->
            { model | mInput = showTerm mTerm }

        _ ->
            model


fillNInputFromRuleTree : RuleTree -> Model -> Model
fillNInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ (App _ nTerm) _ _ _ ->
            { model | nInput = showTerm nTerm }

        _ ->
            model


fillSigmaInputFromRuleTree : RuleTree -> Model -> Model
fillSigmaInputFromRuleTree ruleTree model =
    case ruleTree of
        RVar _ _ sigma _ ->
            { model | sigmaInput = STLC.showType sigma }

        RAbs _ _ (Arrow sigma _) _ ->
            { model | sigmaInput = STLC.showType sigma }

        RApp _ _ _ nextRuleTree1 _ ->
            { model
                | sigmaInput =
                    STLC.getSigmaTypeFromAbsRuleTree nextRuleTree1
                        |> Maybe.map STLC.showType
                        |> Maybe.withDefault ""
            }

        _ ->
            model


fillTauInputFromRuleTree : RuleTree -> Model -> Model
fillTauInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ _ tau _ _ ->
            { model | tauInput = STLC.showType tau }

        RAbs _ _ (Arrow _ tau) _ ->
            { model | tauInput = STLC.showType tau }

        _ ->
            model


fillAllInputsFromRuleTree : RuleTree -> Model -> Model
fillAllInputsFromRuleTree ruleTree =
    flushAllInputs
        >> fillGammaInputFromRuleTree ruleTree
        >> fillXInputFromRuleTree ruleTree
        >> fillMInputFromRuleTree ruleTree
        >> fillNInputFromRuleTree ruleTree
        >> fillSigmaInputFromRuleTree ruleTree
        >> fillTauInputFromRuleTree ruleTree


flushAllInputs : Model -> Model
flushAllInputs model =
    { model | gammaInput = "", xInput = "", mInput = "", nInput = "", sigmaInput = "", tauInput = "" }


{-| Shows the user input elements according to the currently selected Rule.
-}
viewRuleUserInterface : Model -> Html Msg
viewRuleUserInterface model =
    let
        showFor menuStates =
            if List.member model.menuState menuStates then
                style "display" "block"

            else
                style "display" "none"
    in
    div [ class "menu__rule-input-form" ]
        [ div [ showFor [ VarRule, AbsRule, AppRule ] ]
            [ gammaLabel
            , inputBlock gammaInput model (Hint GammaInput)
            ]
        , div [ showFor [ VarRule, AbsRule ] ]
            [ xLabel
            , inputBlock xInput model (Hint XInput)
            ]
        , div [ showFor [ AbsRule, AppRule ] ]
            [ mLabel
            , inputBlock mInput model (Hint MInput)
            ]
        , div [ showFor [ AppRule ] ]
            [ nLabel
            , inputBlock nInput model (Hint NInput)
            ]
        , div [ showFor [ VarRule, AbsRule, AppRule ] ]
            [ sigmaLabel
            , inputBlock sigmaInput model (Hint SigmaInput)
            ]
        , div [ showFor [ AbsRule, AppRule ] ]
            [ tauLabel
            , inputBlock tauInput model (Hint TauInput)
            ]
        , div
            [ if List.member model.menuState [ VarRule, AbsRule, AppRule ] then
                style "display" "flex"

              else
                style "display" "none"
            , class "menu__bottom-button-row"
            ]
            [ button [ onClick Apply ] [ text "Apply" ]
            , div [ style "justify-content" "flex-end" ]
                [ button [ onClick FlushAllInputs ] [ text "🧹" ]
                , button [ onClick FillAllInputs ] [ text "💊" ]
                ]
            ]
        , div [ showFor [ SelectRule ], class "menu__bottom-button-row" ]
            [ div [ style "display" "flex", style "justify-content" "center" ]
                [ button [ onClick (Hint RuleSelection), style "font-size" "120%" ] [ text "💡" ] ]
            ]
        ]


{-| Parses all user inputs and updates the selected RuleTree accordingly.
-}
applyUserInputsToSelectedRuleTreeNode : Model -> Result String RuleTree
applyUserInputsToSelectedRuleTreeNode model =
    let
        maybeContext =
            parseContext model.gammaInput

        gammaErr =
            "Unable to parse the Γ input. Did you forget to put a comma or explicit parantheses for arrow types? Example input: x:a, y:((b->c)->b)"

        xErr =
            "Unable to parse the x input. You should only use lower case latin alphabet characters (a-z). Example input: x"

        sigmaErr =
            "Unable to parse the σ input. Did you forget to put explicit parantheses for arrow types? Example input: ((b->c)->b)"

        tauErr =
            "Unable to parse the τ input. Did you forget to put explicit parantheses for arrow types? Example input: ((b->c)->b)"

        mErr =
            "Unable to parse the M input. Did you forget to put explicit parantheses? Example input: (\\x.(x y))"

        nErr =
            "Unable to parse the N input. Did you forget to put explicit parantheses? Example input: (\\x.(x y))"
    in
    case model.menuState of
        VarRule ->
            case ( maybeContext, parseTermEnd model.xInput, parseTypeEnd model.sigmaInput ) of
                ( Just context, Just xTerm, Just typ ) ->
                    STLC.changeRuleTreeNode model.ruleTree
                        model.selectedNodeId
                        (RVar context
                            xTerm
                            typ
                            True
                        )
                        True
                        |> Ok

                ( Nothing, _, _ ) ->
                    Err gammaErr

                ( _, Nothing, _ ) ->
                    Err xErr

                ( _, _, Nothing ) ->
                    Err sigmaErr

        AbsRule ->
            let
                -- not necessary to handle nextContext as a Maybe type because the other Maybe types cover all invalid input already
                nextContext =
                    STLC.addTypingAssumptionToContext
                        (getFirstCharFromString model.xInput)
                        (parseTypeEnd model.sigmaInput |> Maybe.withDefault (BasicType '#'))
                        (maybeContext |> Maybe.withDefault (Context Dict.empty))

                maybeTerm =
                    parseTerm <| "(λ" ++ model.xInput ++ "." ++ model.mInput ++ ")"

                maybeType =
                    parseType <| "(" ++ model.sigmaInput ++ "→" ++ model.tauInput ++ ")"

                maybeNextMTerm =
                    parseTermEnd model.mInput

                maybeNextType =
                    parseTypeEnd model.tauInput
            in
            -- tuples with more than 3 values are disallowed in elm, so we are using nested tuples here
            case ( maybeContext, maybeTerm, ( maybeType, maybeNextMTerm, maybeNextType ) ) of
                ( Just context, Just term, ( Just typ, Just nextMTerm, Just nextType ) ) ->
                    STLC.changeRuleTreeNode model.ruleTree
                        model.selectedNodeId
                        (RAbs context
                            term
                            typ
                            (createRuleTree nextContext nextMTerm nextType)
                        )
                        True
                        |> Ok

                ( Nothing, _, ( _, _, _ ) ) ->
                    Err gammaErr

                ( _, Nothing, ( _, Just _, _ ) ) ->
                    Err xErr

                ( _, _, ( Nothing, _, Just _ ) ) ->
                    Err sigmaErr

                ( _, _, ( _, Nothing, _ ) ) ->
                    Err mErr

                ( _, _, ( _, _, Nothing ) ) ->
                    Err tauErr

        AppRule ->
            let
                maybeMTerm =
                    parseTermEnd model.mInput

                maybeSigmaType =
                    parseTypeEnd model.sigmaInput

                maybeTauType =
                    parseTypeEnd model.tauInput

                maybeNTerm =
                    parseTermEnd model.nInput
            in
            -- tuples with more than 3 values are disallowed in elm, so we are using nested tuples here
            case ( maybeContext, maybeMTerm, ( maybeNTerm, maybeSigmaType, maybeTauType ) ) of
                ( Just context, Just mTerm, ( Just nTerm, Just sigmaType, Just tauType ) ) ->
                    STLC.changeRuleTreeNode
                        model.ruleTree
                        model.selectedNodeId
                        (RApp context
                            (parseTermEnd ("(" ++ model.mInput ++ " " ++ model.nInput ++ ")") |> Maybe.withDefault (Var '#'))
                            tauType
                            (createRuleTree context mTerm (Arrow sigmaType tauType))
                            (createRuleTree context nTerm sigmaType)
                        )
                        True
                        |> Ok

                ( Nothing, _, ( _, _, _ ) ) ->
                    Err gammaErr

                ( _, Nothing, ( _, _, _ ) ) ->
                    Err mErr

                ( _, _, ( Nothing, _, _ ) ) ->
                    Err nErr

                ( _, _, ( _, Nothing, _ ) ) ->
                    Err sigmaErr

                ( _, _, ( _, _, Nothing ) ) ->
                    Err tauErr

        _ ->
            Err "Unexpected error. The menu state was unexpectedly not set to an inference rule."


adjustMenuStateToSelectedRuleTree : Model -> Model
adjustMenuStateToSelectedRuleTree model =
    let
        currentSelectedRuleTree =
            getSelectedRuleTreeNode model
    in
    case currentSelectedRuleTree of
        RVar _ _ _ True ->
            changeState VarRule model

        RVar _ _ _ False ->
            changeState SelectRule model

        RAbs _ _ _ nextRuleTree ->
            if nextRuleTree == Hole then
                changeState SelectRule model

            else
                changeState AbsRule model

        RApp _ _ _ nextRuleTree1 nextRuleTree2 ->
            if nextRuleTree1 == Hole && nextRuleTree2 == Hole then
                changeState SelectRule model

            else
                changeState AppRule model

        Hole ->
            changeState SelectRule model


gammaLabel : Html msg
gammaLabel =
    label [ for "gammaInput" ] [ text "Γ" ]


gammaInput : Model -> Html Msg
gammaInput model =
    newInput model.gammaInput "gammaInput" Gamma


xLabel : Html msg
xLabel =
    label [ for "xInput" ] [ text "x" ]


xInput : Model -> Html Msg
xInput model =
    newInput model.xInput "xInput" X


mLabel : Html msg
mLabel =
    label [ for "mInput", class "capitalLetterLabel" ] [ text "M" ]


mInput : Model -> Html Msg
mInput model =
    newInput model.mInput "mInput" M


nLabel : Html msg
nLabel =
    label [ for "nInput", class "capitalLetterLabel" ] [ text "N" ]


nInput : Model -> Html Msg
nInput model =
    newInput model.nInput "nInput" N


sigmaLabel : Html msg
sigmaLabel =
    label [ for "sigmaInput" ] [ text "σ" ]


sigmaInput : Model -> Html Msg
sigmaInput model =
    newInput model.sigmaInput "sigmaInput" Sigma


tauLabel : Html msg
tauLabel =
    label [ for "tauInput" ] [ text "τ" ]


tauInput : Model -> Html Msg
tauInput model =
    newInput model.tauInput "tauInput" Tau



--    unicode characters:    → λ σ τ Γ ⊢


{-| If possible, transforms a Char to its representation we use for typing, otherwise the Char remains unchanged.
-}
charToTypingRepresentation : Char -> Char
charToTypingRepresentation char =
    case char of
        '\\' ->
            'λ'

        'a' ->
            'α'

        'b' ->
            'β'

        'c' ->
            'ς'

        'd' ->
            'δ'

        'e' ->
            'ε'

        'f' ->
            'ξ'

        'g' ->
            'γ'

        'h' ->
            'ψ'

        'i' ->
            'ι'

        'j' ->
            'Ξ'

        'k' ->
            'κ'

        'l' ->
            'Λ'

        'm' ->
            'μ'

        'n' ->
            'η'

        'o' ->
            'ο'

        'p' ->
            'ρ'

        'q' ->
            'Δ'

        'r' ->
            'Γ'

        's' ->
            'σ'

        't' ->
            'τ'

        'u' ->
            'υ'

        'v' ->
            'ν'

        'w' ->
            'ω'

        'x' ->
            'χ'

        'y' ->
            'Υ'

        'z' ->
            'ζ'

        _ ->
            char


{-| Used for Strings that are supposed to be a present _just_ a type.
-}
stringToTypingRepresantation : String -> String
stringToTypingRepresantation str =
    String.map charToTypingRepresentation str
        |> String.replace "->" "→"


{-| For every typing assumption of the form `x:t` where x is an arbitrary variable
and t an arbitrary type containing standard keyboard characters (e.g. `(a->(b->a))`,
the function converts it to `x:T` where T is the typing representation of t (e.g. `(α→(β→α))`).

**Example**

    stringOfTypingAssumptionsToTypingRepresantation "x:(a->(b->a)), y:s" => "x:(α→(β→α)),y:σ"

-}
stringOfTypingAssumptionsToTypingRepresantation : String -> String
stringOfTypingAssumptionsToTypingRepresantation str =
    let
        splitUp str1 =
            String.split "," str1 |> List.concatMap (String.split ":")

        joinUpAndConvert splittedParts beforeColon =
            case splittedParts of
                lastElem :: [] ->
                    stringToTypingRepresantation lastElem

                head :: tail ->
                    if beforeColon then
                        head ++ ":" ++ joinUpAndConvert tail False

                    else
                        stringToTypingRepresantation head ++ "," ++ joinUpAndConvert tail True

                _ ->
                    ""
    in
    joinUpAndConvert (String.replace " " "" str |> splitUp) True



-- PARSING


parseVar : Parser Char
parseVar =
    Parser.chompIf isValidVariableInput |> Parser.getChompedString |> Parser.map getFirstCharFromString


parseTypeVar : Parser Char
parseTypeVar =
    Parser.chompIf isValidTypeVarInput |> Parser.getChompedString |> Parser.map getFirstCharFromString


{-| Used to parse the RuleTree in the proofterm query of the URL.
-}
ruleTreeParser : Parser RuleTree
ruleTreeParser =
    Parser.oneOf
        [ Parser.succeed RVar
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'V'))
            |. Parser.symbol "_"
            |= contextParser
            |= termParser
            |= typeParser
            |= boolParser
        , Parser.succeed RAbs
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'A'))
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'B'))
            |. Parser.symbol "_"
            |= contextParser
            |= termParser
            |= typeParser
            |= Parser.lazy (\_ -> ruleTreeParser)
        , Parser.succeed RApp
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'A'))
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'P'))
            |. Parser.symbol "_"
            |= contextParser
            |= termParser
            |= typeParser
            |= Parser.lazy (\_ -> ruleTreeParser)
            |= Parser.lazy (\_ -> ruleTreeParser)
        , Parser.succeed Hole
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'H'))
            |. Parser.symbol "_"
        ]


parseRuleTree : String -> Maybe RuleTree
parseRuleTree str =
    Result.toMaybe <| Parser.run ruleTreeParser str


boolParser : Parser Bool
boolParser =
    Parser.oneOf
        [ Parser.succeed True
            |. Parser.chompIf (\c -> c == 'T')
        , Parser.succeed False
            |. Parser.chompIf (\c -> c == 'F')
        ]


termParser : Parser Term
termParser =
    Parser.oneOf
        [ Parser.succeed Var
            |= parseVar
        , Parser.succeed Abs
            |. (Parser.backtrackable <| Parser.symbol "(")
            |. (Parser.backtrackable <| Parser.symbol "λ")
            |= parseVar
            |. Parser.symbol "."
            |= Parser.lazy (\_ -> termParser)
            |. Parser.symbol ")"
        , Parser.succeed App
            |. Parser.symbol "("
            |= Parser.lazy (\_ -> termParser)
            |. Parser.symbol " "
            |= Parser.lazy (\_ -> termParser)
            |. Parser.symbol ")"
        ]


termParserEnd : Parser Term
termParserEnd =
    Parser.succeed identity
        |= termParser
        |. Parser.end



{- parseTermEnd : String -> Maybe Term
   parseTermEnd str =
       case Parser.run (Parser.backtrackable termParserEnd) str of
           Ok term ->
               Just term

           Err _ ->
               Nothing
-}


parseTermEnd : String -> Maybe Term
parseTermEnd str =
    Parser.run (Parser.backtrackable termParserEnd) str |> Result.toMaybe


parseTerm : String -> Maybe Term
parseTerm str =
    Result.toMaybe <| Parser.run (Parser.backtrackable termParser) str


typingAssumptionParser : Parser ( Shared.Var, Shared.SType )
typingAssumptionParser =
    Parser.succeed Tuple.pair
        |= parseVar
        |. Parser.symbol ":"
        |= typeParser


{-| Auxiliary parser; required for parsing an arbitrary amount of typing assumptions from the Context into List format.
Use parseContext or contextParser to get the Context format.
-}
contextAsListParser : Parser (List ( Shared.Var, Shared.SType ))
contextAsListParser =
    Parser.sequence
        { start = "{"
        , separator = ","
        , end = "}"
        , spaces = Parser.spaces
        , item = typingAssumptionParser
        , trailing = Parser.Optional
        }


{-| Used to parse the gamma text input.
-}
parseContext : String -> Maybe SContext
parseContext str =
    Parser.run contextAsListParser ("{" ++ str ++ "}") |> Result.toMaybe |> Maybe.map Dict.fromList |> Maybe.map Context


{-| Used to parse Contexts in the proofterm query of the URL.
-}
contextParser : Parser SContext
contextParser =
    Parser.map (\list -> Context <| Dict.fromList list) contextAsListParser


typeParser : Parser Shared.SType
typeParser =
    Parser.oneOf
        [ Parser.succeed Shared.BasicType
            |= parseTypeVar
        , Parser.succeed Shared.Arrow
            |. Parser.symbol "("
            |= Parser.lazy (\_ -> typeParser)
            |. Parser.symbol "→"
            |= Parser.lazy (\_ -> typeParser)
            |. Parser.symbol ")"
        , Parser.succeed Shared.Untyped
            |. Parser.symbol "?"
        ]


typeParserEnd : Parser SType
typeParserEnd =
    Parser.succeed identity
        |= typeParser
        |. Parser.end


parseType : String -> Maybe SType
parseType str =
    Parser.run typeParser str |> Result.toMaybe


parseTypeEnd : String -> Maybe SType
parseTypeEnd str =
    Parser.run typeParserEnd str |> Result.toMaybe


validVarAndTypeVarInputs : List Char
validVarAndTypeVarInputs =
    [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]


isValidTypeVarInput : Char -> Bool
isValidTypeVarInput char =
    let
        validGreekTypeVarNames =
            List.map charToTypingRepresentation validVarAndTypeVarInputs

        validTypeVarInputs =
            validVarAndTypeVarInputs ++ validGreekTypeVarNames
    in
    List.member char validTypeVarInputs


isValidVariableInput : Char -> Bool
isValidVariableInput char =
    List.member char validVarAndTypeVarInputs


getFirstCharFromString : String -> Char
getFirstCharFromString str =
    case String.uncons str of
        Just ( char, _ ) ->
            char

        Nothing ->
            '#'
