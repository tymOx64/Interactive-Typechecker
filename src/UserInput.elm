module UserInput exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Parser exposing ((|.), (|=), Parser)
import RuleTree exposing (addTypingAssumptionToContext, changeRuleTreeNode, createRuleTree, getContextFromRuleTree, getLeftTypeFromRuleTree, getSelectedRuleTreeNode, showContext, showTerm, showTermVar, showType)
import Set
import SharedStructures as Shared exposing (..)


{-| Creates a new labeled text input field. When losing focus, triggers the Msg `TransformInput`.
Stops propagation of `KeyDown` events (e.g. to allow arrow keys move the cursor within the text input).

`val`: starting string in the input field

`id`: to which label the input belongs to

`toMsg`: what Msg gets triggered when typing a character in the input

-}
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


{-| Creates a simple button.

`txt`: the text label to be shown on the button itself

`msg`: the Msg to be triggered when the button gets clicked

-}
newButton : String -> msg -> Html msg
newButton txt msg =
    button [ onClick msg ] [ text txt ]


{-| Creates an input block consisting of a text input along with its hint button.

`textInput`: the text input field

`model`: the model

`msg`: the Msg to be triggered when the hint button gets clicked

-}
inputBlock : (Model -> Html Msg) -> Model -> Msg -> Html Msg
inputBlock textInput model msg =
    div [ class "menu__meta-variable-input-block" ]
        [ textInput model
        , button [ onClick msg ] [ text "💡" ]
        ]


{-| Views the gamma input and its label for the init screen.
-}
viewContextInitBlock : Model -> Html Msg
viewContextInitBlock model =
    div [ class "init-starting-node__input-block" ]
        [ label [ for "gammaInput", class "capital-letter-label" ] [ text "Γ" ]
        , newInput model.gammaInput "gammaInput" Gamma
        ]


{-| Views the term input (M) and its label for the init screen.
-}
viewTermInitBlock : Model -> Html Msg
viewTermInitBlock model =
    div [ class "init-starting-node__input-block" ]
        [ label [ for "mInput", class "capital-letter-label" ] [ text "M" ]
        , newInput model.mInput "mInput" M
        ]


{-| Views the type input (tau) and its label for the init screen.
-}
viewTypeInitBlock : Model -> Html Msg
viewTypeInitBlock model =
    div [ class "init-starting-node__input-block" ]
        [ label [ for "tauInput", class "capital-letter-label" ] [ text "τ" ]
        , newInput model.tauInput "tauInput" Tau
        ]


{-| Views the three inputs `Γ, M, τ` for the init screen.
-}
viewNodeInitiationInputs : Model -> Html Msg
viewNodeInitiationInputs model =
    div [ class "init-starting-node__input-block-container" ]
        [ viewContextInitBlock model
        , viewTermInitBlock model
        , viewTypeInitBlock model
        ]


{-| View the _Get URL_ and _Start_ button for the home screen.
-}
viewNodeInitiationButtons : Html Msg
viewNodeInitiationButtons =
    div [ class "init-starting-node__button-block-container" ]
        [ button [ onClick GetUrl, class "init-starting-node__button-block" ] [ text "Get URL 🌐" ]
        , button [ onClick Start, class "init-starting-node__button-block" ] [ text "Start 🚀" ]
        ]


{-| Fills the gamma input from given `ruleTree`.
-}
fillGammaInputFromRuleTree : RuleTree -> Model -> Model
fillGammaInputFromRuleTree ruleTree model =
    let
        contextAsString =
            getContextFromRuleTree ruleTree |> showContext
    in
    { model | gammaInput = contextAsString }


{-| Fills the x input from given `ruleTree`.
-}
fillXInputFromRuleTree : RuleTree -> Model -> Model
fillXInputFromRuleTree ruleTree model =
    case ruleTree of
        RVar _ (Var var) _ _ ->
            { model | xInput = showTermVar var }

        RAbs _ (Abs var _) _ _ ->
            { model | xInput = showTermVar var }

        _ ->
            model


{-| Fills the M input from given `ruleTree`.
-}
fillMInputFromRuleTree : RuleTree -> Model -> Model
fillMInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ (App mTerm _) _ _ _ ->
            { model | mInput = showTerm mTerm }

        RAbs _ (Abs _ mTerm) _ _ ->
            { model | mInput = showTerm mTerm }

        _ ->
            model


{-| Fills the N input from given `ruleTree`.
-}
fillNInputFromRuleTree : RuleTree -> Model -> Model
fillNInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ (App _ nTerm) _ _ _ ->
            { model | nInput = showTerm nTerm }

        _ ->
            model


{-| Fills the sigma input from given `ruleTree`.
-}
fillSigmaInputFromRuleTree : RuleTree -> Model -> Model
fillSigmaInputFromRuleTree ruleTree model =
    case ruleTree of
        RVar _ _ sigma _ ->
            { model | sigmaInput = showType sigma }

        RAbs _ _ (Arrow sigma _) _ ->
            { model | sigmaInput = showType sigma }

        RApp _ _ _ childRuleTree1 _ ->
            { model
                | sigmaInput =
                    getLeftTypeFromRuleTree childRuleTree1
                        |> Maybe.map showType
                        |> Maybe.withDefault ""
            }

        _ ->
            model


{-| Fills the tau input from given `ruleTree`.
-}
fillTauInputFromRuleTree : RuleTree -> Model -> Model
fillTauInputFromRuleTree ruleTree model =
    case ruleTree of
        RApp _ _ tau _ _ ->
            { model | tauInput = showType tau }

        RAbs _ _ (Arrow _ tau) _ ->
            { model | tauInput = showType tau }

        _ ->
            model


{-| Fills all inputs from given `ruleTree`.
-}
fillAllInputsFromRuleTree : RuleTree -> Model -> Model
fillAllInputsFromRuleTree ruleTree =
    flushAllInputs
        >> fillGammaInputFromRuleTree ruleTree
        >> fillXInputFromRuleTree ruleTree
        >> fillMInputFromRuleTree ruleTree
        >> fillNInputFromRuleTree ruleTree
        >> fillSigmaInputFromRuleTree ruleTree
        >> fillTauInputFromRuleTree ruleTree


{-| Clears all inputs to be fully empty.
-}
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
                , button [ onClick ApplyLatestChangesToFullRuleTree ] [ text "♻️" ]
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
            parseContext (String.replace " " "" model.gammaInput)

        gammaErr =
            "Unable to parse the Γ input. Did you forget to put explicit parantheses for arrow types? Example input: x:a, y:(b->c)->b"

        xErr =
            "Unable to parse the x input. You should only use latin alphabet characters (a-z, A-Z). Example input: x"

        sigmaErr =
            "Unable to parse the σ input. Did you forget to put explicit parantheses for arrow types? Example input: (b->c)->b"

        tauErr =
            "Unable to parse the τ input. Did you forget to put explicit parantheses for arrow types? Example input: (b->c)->b"

        mErr =
            "Unable to parse the M input. Did you forget to put explicit parantheses? Example input: \\x.(x y)"

        nErr =
            "Unable to parse the N input. Did you forget to put explicit parantheses? Example input: \\x.(x y)"
    in
    case model.menuState of
        VarRule ->
            case ( maybeContext, parseTermEnd model.xInput, parseTypeEnd model.sigmaInput ) of
                ( Just context, Just xTerm, Just typ ) ->
                    changeRuleTreeNode model.ruleTree
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
                -- not necessary to handle childContext as a Maybe type because the other Maybe types cover all invalid input already
                childContext =
                    addTypingAssumptionToContext
                        (getFirstCharFromString model.xInput)
                        (parseTypeEnd model.sigmaInput |> Maybe.withDefault (BasicType "#"))
                        (maybeContext |> Maybe.withDefault (Context Dict.empty))

                maybeXVar =
                    parseTermEnd model.xInput

                maybeMTerm =
                    parseTermEnd model.mInput

                maybeSigmaType =
                    parseTypeEnd model.sigmaInput

                maybeTauType =
                    parseTypeEnd model.tauInput
            in
            -- tuples with more than 3 values are disallowed in elm, so we are using nested tuples here
            case ( maybeContext, maybeXVar, ( maybeMTerm, maybeSigmaType, maybeTauType ) ) of
                ( Just context, Just (Var xVar), ( Just mTerm, Just sigmaType, Just tauType ) ) ->
                    changeRuleTreeNode model.ruleTree
                        model.selectedNodeId
                        (RAbs context
                            (Abs xVar mTerm)
                            (Arrow sigmaType tauType)
                            (createRuleTree childContext mTerm tauType)
                        )
                        True
                        |> Ok

                ( Nothing, _, ( _, _, _ ) ) ->
                    Err gammaErr

                ( _, Nothing, ( _, _, _ ) ) ->
                    Err xErr

                ( _, _, ( Nothing, _, _ ) ) ->
                    Err mErr

                ( _, _, ( _, Nothing, _ ) ) ->
                    Err sigmaErr

                ( _, _, ( _, _, Nothing ) ) ->
                    Err tauErr

                -- xInput is a valid term, but not a Variable term
                ( _, Just _, ( _, _, _ ) ) ->
                    Err xErr

        AppRule ->
            let
                maybeMTerm =
                    parseTermEnd model.mInput

                maybeNTerm =
                    parseTermEnd model.nInput

                maybeSigmaType =
                    parseTypeEnd model.sigmaInput

                maybeTauType =
                    parseTypeEnd model.tauInput
            in
            -- tuples with more than 3 values are disallowed in elm, so we are using nested tuples here
            case ( maybeContext, maybeMTerm, ( maybeNTerm, maybeSigmaType, maybeTauType ) ) of
                ( Just context, Just mTerm, ( Just nTerm, Just sigmaType, Just tauType ) ) ->
                    changeRuleTreeNode
                        model.ruleTree
                        model.selectedNodeId
                        (RApp context
                            (App mTerm nTerm)
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


applyUserInitInputs : Model -> Result String RuleTree
applyUserInitInputs model =
    let
        maybeContext =
            parseContext (String.replace " " "" model.gammaInput)

        maybeMTerm =
            parseTermEnd model.mInput

        maybeTauType =
            if model.tauInput == "" then
                Just Untyped

            else
                parseTypeEnd model.tauInput

        gammaErr =
            "Unable to parse the Γ input. Did you forget to put explicit parantheses for arrow types? Example input: x:a, y:(b->c)->b"

        tauErr =
            "Unable to parse the τ input. Did you forget to put explicit parantheses for arrow types? Example input: (b->c)->b"

        mErr =
            "Unable to parse the M input. Did you forget to put explicit parantheses? Example input: \\x.(x y)"
    in
    case ( maybeContext, maybeMTerm, maybeTauType ) of
        ( Just context, Just mTerm, Just tauType ) ->
            (case mTerm of
                Var _ ->
                    RVar context mTerm tauType False

                Abs _ _ ->
                    RAbs context mTerm tauType Hole

                App _ _ ->
                    RApp context mTerm tauType Hole Hole
            )
                |> Ok

        ( Nothing, _, _ ) ->
            Err gammaErr

        ( _, Nothing, _ ) ->
            Err mErr

        ( _, _, Nothing ) ->
            Err tauErr


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

        RAbs _ _ _ childRuleTree ->
            if childRuleTree == Hole then
                changeState SelectRule model

            else
                changeState AbsRule model

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            if childRuleTree1 == Hole && childRuleTree2 == Hole then
                changeState SelectRule model

            else
                changeState AppRule model

        Hole ->
            changeState SelectRule model


{-| Label for the gamma input.
-}
gammaLabel : Html msg
gammaLabel =
    label [ for "gammaInput" ] [ text "Γ" ]


{-| Creates the text input field for gamma.
-}
gammaInput : Model -> Html Msg
gammaInput model =
    newInput model.gammaInput "gammaInput" Gamma


{-| Label for the x input.
-}
xLabel : Html msg
xLabel =
    label [ for "xInput" ] [ text "x" ]


{-| Creates the text input field for x.
-}
xInput : Model -> Html Msg
xInput model =
    newInput model.xInput "xInput" X


{-| Label for the M input.
-}
mLabel : Html msg
mLabel =
    label [ for "mInput", class "capital-letter-label" ] [ text "M" ]


{-| Creates the text input field for M.
-}
mInput : Model -> Html Msg
mInput model =
    newInput model.mInput "mInput" M


{-| Label for the N input.
-}
nLabel : Html msg
nLabel =
    label [ for "nInput", class "capital-letter-label" ] [ text "N" ]


{-| Creates the text input field for N.
-}
nInput : Model -> Html Msg
nInput model =
    newInput model.nInput "nInput" N


{-| Label for the sigma input.
-}
sigmaLabel : Html msg
sigmaLabel =
    label [ for "sigmaInput" ] [ text "σ" ]


{-| Creates the text input field for sigma.
-}
sigmaInput : Model -> Html Msg
sigmaInput model =
    newInput model.sigmaInput "sigmaInput" Sigma


{-| Label for the tau input.
-}
tauLabel : Html msg
tauLabel =
    label [ for "tauInput" ] [ text "τ" ]


{-| Creates the text input field for tau.
-}
tauInput : Model -> Html Msg
tauInput model =
    newInput model.tauInput "tauInput" Tau



--    unicode characters:    → λ σ τ Γ ⊢
-- PARSING


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
            |. Parser.symbol "$"
            |= typeParser
            |. Parser.symbol "$"
            |= boolParser
        , Parser.succeed RAbs
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'A'))
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'B'))
            |. Parser.symbol "_"
            |= contextParser
            |= termParser
            |. Parser.symbol "$"
            |= typeParser
            |. Parser.symbol "$"
            |= Parser.lazy (\_ -> ruleTreeParser)
        , Parser.succeed RApp
            |. (Parser.backtrackable <| Parser.symbol "_")
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'A'))
            |. (Parser.backtrackable <| Parser.chompIf (\c -> c == 'P'))
            |. Parser.symbol "_"
            |= contextParser
            |= termParser
            |. Parser.symbol "$"
            |= typeParser
            |. Parser.symbol "$"
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


parseTermVar : Parser Char
parseTermVar =
    Parser.chompIf Char.isAlpha |> Parser.getChompedString |> Parser.map getFirstCharFromString


{-| Parses terms. Outmost parantheses may be omitted, e.g. it works both for
`"(x y)"` and `"x y"` to be parsed to `App (Var "x") (Var "y")`.
-}
termParser : Parser Term
termParser =
    Parser.oneOf
        [ Parser.succeed App
            |= (Parser.backtrackable <| termParserInner)
            |. (Parser.backtrackable <| Parser.symbol " ")
            |= (Parser.backtrackable <| termParserInner)
        , Parser.succeed Abs
            |. Parser.symbol "λ"
            |= parseTermVar
            |. Parser.symbol "."
            |= termParserInner
        , termParserInner
        ]


{-| Parses inner terms, i.e. either plain Variables, or Abs and App that are put into parantheses.
-}
termParserInner : Parser Term
termParserInner =
    Parser.oneOf
        [ Parser.succeed Abs
            |. (Parser.backtrackable <| Parser.symbol "(")
            |. (Parser.backtrackable <| Parser.symbol "λ")
            |= parseTermVar
            |. Parser.symbol "."
            |= Parser.lazy (\_ -> termParserInner)
            |. Parser.symbol ")"
        , Parser.succeed App
            |. Parser.symbol "("
            |= Parser.lazy (\_ -> termParserInner)
            |. Parser.symbol " "
            |= Parser.lazy (\_ -> termParserInner)
            |. Parser.symbol ")"
        , Parser.succeed Var
            |= (Parser.backtrackable <| parseTermVar)
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


typingAssumptionParser : Parser ( Shared.TermVar, Shared.SType )
typingAssumptionParser =
    Parser.succeed Tuple.pair
        |= parseTermVar
        |. Parser.symbol ":"
        |= typeParser


{-| Auxiliary parser; required for parsing an arbitrary amount of typing assumptions from the Context into List format.
Use parseContext or contextParser to get the Context format.
-}
contextAsListParser : Parser (List ( TermVar, SType ))
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


parseTypeVar : Parser String
parseTypeVar =
    Parser.variable { start = Char.isAlpha, inner = \c -> Char.isAlphaNum c || c == '\'' || c == '_', reserved = Set.empty }


typeParser : Parser SType
typeParser =
    Parser.oneOf
        [ Parser.succeed Arrow
            |= (Parser.backtrackable <| Parser.lazy (\_ -> typeParserInner))
            |. Parser.spaces
            |. (Parser.backtrackable <| Parser.symbol "→")
            |. Parser.spaces
            |= Parser.lazy (\_ -> typeParserInner)
        , typeParserInner
        ]


typeParserInner : Parser SType
typeParserInner =
    Parser.oneOf
        [ Parser.succeed BasicType
            |= parseTypeVar
        , Parser.succeed Arrow
            |. Parser.symbol "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> typeParserInner)
            |. Parser.spaces
            |. Parser.symbol "→"
            |. Parser.spaces
            |= Parser.lazy (\_ -> typeParserInner)
            |. Parser.spaces
            |. Parser.symbol ")"
        , Parser.succeed Untyped
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


lowerCaseLatinAlphabet : List Char
lowerCaseLatinAlphabet =
    List.range (Char.toCode 'a') (Char.toCode 'z') |> List.map Char.fromCode


{-| Returns the first char from a string if possible, otherise `'#'`.

This is currently not returning a Maybe type to simplify its usage with `Parser`.
Returning `'#'` as a faulty state needs to get handled seperately.

-}
getFirstCharFromString : String -> Char
getFirstCharFromString str =
    case String.uncons str of
        Just ( char, _ ) ->
            char

        Nothing ->
            '#'
