port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation exposing (pushUrl)
import Dict
import Hint exposing (applyLatestTermVarTypingsToFullRuleTree, getHint, passChangesThroughRuleTree, updateLatestTermVarTypings)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Regex
import RuleTree exposing (encodeRuleTreeAsString, generateDisplayMessageIfRuleTreeIsSuccessfulBesidesContextMissingFreeVar, getFirstConflictFromRuleTree, getNodeIDForArrowDownKeyEvent, getNodeIDForArrowLeftOrRightKeyEvent, getNodeIDForArrowUpKeyEvent, resetRuleTreeNode, ruleTreeIsSuccessful, viewAbstractionRule, viewApplicationRule, viewRuleTree, viewVarRule)
import RuleTreeUtils exposing (getRuleTreeNode, getSelectedRuleTreeNode, getTermFromRuleTree)
import Set
import SharedStructures exposing (..)
import Url
import Url.Parser
import Url.Parser.Query as Query
import UserInput exposing (..)



-- MAIN


main : Program String Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


init : String -> ( Model, Cmd Msg )
init locationHref =
    let
        ruleTreeFromUrlQuery =
            getRuleTreeFromUrlQuery locationHref

        noProoftreeQueryGiven =
            getUrlQuery "prooftree" (fixLocalUrl locationHref) |> (==) Nothing

        initialRuleTree =
            ruleTreeFromUrlQuery |> Maybe.withDefault Hole

        _ =
            Debug.log "test stuff" <| 5
    in
    ( { viewState =
            if noProoftreeQueryGiven then
                Start

            else
                SelectRule
      , ruleTree = initialRuleTree
      , selectedNodeID = []
      , latestTermVarTypings = Dict.empty
      , gammaInput = ""
      , xInput = ""
      , mInput = ""
      , nInput = ""
      , sigmaInput = ""
      , tauInput = ""
      , displayMessage =
            case ( ruleTreeFromUrlQuery, getBaseUrl locationHref, noProoftreeQueryGiven ) of
                ( Just _, Just _, False ) ->
                    "Welcome to the interactive STLC Typechecker. Start by selecting an inference rule right above this message!"

                ( Nothing, _, False ) ->
                    "Parsing Error on the prooftree query. Did you copy the full URL?"

                ( _, Nothing, False ) ->
                    "Unable to parse the URL. The application needs to be launched from an html file!"

                -- for the start menu, don't show any display message initially
                ( _, _, True ) ->
                    ""
      , ruleTreeSuccessful = ruleTreeIsSuccessful initialRuleTree (getFirstConflictFromRuleTree initialRuleTree)
      , baseUrl = getBaseUrl locationHref |> Maybe.withDefault "https://www.typeCheckerDummyUrl.com/"
      , viewLatinChar = False
      }
    , Cmd.none
    )


getUrlWithProoftree : Model -> RuleTree -> String
getUrlWithProoftree model ruleTree =
    model.baseUrl ++ "?prooftree=" ++ encodeRuleTreeAsString ruleTree


getRuleTreeFromUrlQuery : String -> Maybe RuleTree
getRuleTreeFromUrlQuery urlAsString =
    Maybe.andThen parseRuleTree <| getUrlQuery "prooftree" <| fixLocalUrl urlAsString


{-| Transforms a local URL to a common web URL since Elms current URL packages can't handle local URLs very well (as of 21st August 2021).
-}
fixLocalUrl : String -> String
fixLocalUrl =
    case Regex.fromString "file.*[.]html" of
        Nothing ->
            identity

        Just regex ->
            Regex.replace regex (\_ -> "https://www.typeCheckerDummyUrl.com/")


{-| Reduces the URL to its beginning parts `scheme`, `authority` and `path`, i.e. it cuts off the parts `query` and `fragment`.
-}
getBaseUrl : String -> Maybe String
getBaseUrl str =
    {- let
           _ =
               Debug.log "getBaseUrl" (Regex.find (Maybe.withDefault Regex.never (Regex.fromString ".*[.]html")) str |> List.map .match |> List.head |> Maybe.withDefault "gg")
       in
    -}
    Regex.find (Maybe.withDefault Regex.never (Regex.fromString ".*[.]html")) str |> List.map .match |> List.head


getUrlQuery : String -> String -> Maybe String
getUrlQuery query urlAsString =
    -- the leftmost call just flattens the nested Maybe value
    Maybe.withDefault Nothing <| Maybe.andThen (Url.Parser.parse <| Url.Parser.query <| Query.string query) <| Url.fromString urlAsString



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update log" msg

        --Debug.log "unusedTypeVar:" <| List.map (\x -> getUnusedTypeVariableFromRuleTree model.ruleTree x) (List.range 1 96)
        varShadowingErrMsg =
            "The Term M you have entered contains variable shadowing which is not supported. Please rename variables to prevent shadowing."
    in
    case msg of
        Gamma str ->
            ( { model | gammaInput = str }, Cmd.none )

        X str ->
            ( { model | xInput = str }, Cmd.none )

        M str ->
            ( { model | mInput = str }, Cmd.none )

        N str ->
            ( { model | nInput = str }, Cmd.none )

        Sigma str ->
            ( { model | sigmaInput = str }, Cmd.none )

        Tau str ->
            ( { model | tauInput = str }, Cmd.none )

        Hint inputKind ->
            ( getHint inputKind model, Cmd.none )

        TransformInput ->
            ( { model
                | gammaInput = String.replace "->" "→" model.gammaInput
                , mInput = String.replace "\\" "λ" model.mInput
                , nInput = String.replace "\\" "λ" model.nInput
                , sigmaInput = String.replace "->" "→" model.sigmaInput
                , tauInput = String.replace "->" "→" model.tauInput
              }
            , Cmd.none
            )

        FillAllInputs ->
            ( UserInput.fillAllInputsFromRuleTree (getSelectedRuleTreeNode model) model, Cmd.none )

        FlushAllInputs ->
            ( UserInput.flushAllInputs model, Cmd.none )

        ApplyLatestChangesToFullRuleTree ->
            let
                ruleTreeWithLatestTermVarTypings =
                    applyLatestTermVarTypingsToFullRuleTree model.latestTermVarTypings model.ruleTree

                currentlySelectedRuleTree =
                    getRuleTreeNode ruleTreeWithLatestTermVarTypings model.selectedNodeID
            in
            ( model, pushUrl <| getUrlWithProoftree model <| passChangesThroughRuleTree currentlySelectedRuleTree model.selectedNodeID ruleTreeWithLatestTermVarTypings )

        Apply ->
            case UserInput.applyUserInputsToSelectedRuleTreeNode model of
                Ok ruleTree ->
                    ( flushAllInputs model, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        SelectTreeNode nodeID ->
            ( adjustViewStateToSelectedRuleTree { model | selectedNodeID = nodeID }, Cmd.none )

        ResetTreeNode nodeID ->
            ( adjustViewStateToSelectedRuleTree model
            , pushUrl <| getUrlWithProoftree model (resetRuleTreeNode model.ruleTree nodeID)
            )

        ChangeViewState newState ->
            ( changeViewState newState model, Cmd.none )

        KeyDown key ->
            let
                cmd =
                    if key == "Enter" then
                        case UserInput.applyUserInputsToSelectedRuleTreeNode model of
                            Ok ruleTree ->
                                pushUrl <| getUrlWithProoftree model ruleTree

                            Err _ ->
                                Cmd.none

                    else if key == "Delete" then
                        pushUrl <| getUrlWithProoftree model (resetRuleTreeNode model.ruleTree model.selectedNodeID)

                    else
                        Cmd.none
            in
            ( if key == "ArrowUp" then
                { model | selectedNodeID = getNodeIDForArrowUpKeyEvent model.ruleTree model.selectedNodeID }
                    |> adjustViewStateToSelectedRuleTree

              else if key == "ArrowDown" then
                { model | selectedNodeID = getNodeIDForArrowDownKeyEvent model.ruleTree model.selectedNodeID }
                    |> adjustViewStateToSelectedRuleTree

              else if key == "ArrowLeft" then
                { model | selectedNodeID = getNodeIDForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeID True }
                    |> adjustViewStateToSelectedRuleTree

              else if key == "ArrowRight" then
                { model | selectedNodeID = getNodeIDForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeID False }
                    |> adjustViewStateToSelectedRuleTree

              else if key == "1" then
                changeViewState VarRule model

              else if key == "2" then
                changeViewState AppRule model

              else if key == "3" then
                changeViewState AbsRule model

              else if key == "Enter" then
                -- the actual application happens through the cmd (see "let in" above)
                case UserInput.applyUserInputsToSelectedRuleTreeNode model of
                    Ok _ ->
                        adjustViewStateToSelectedRuleTree model
                            |> flushAllInputs

                    Err err ->
                        { model | displayMessage = err }

              else if key == "Delete" then
                -- the actual deletion happens through the cmd (see "let in" above)
                adjustViewStateToSelectedRuleTree model

              else
                model
            , cmd
            )

        UrlChanged newUrl ->
            case getRuleTreeFromUrlQuery newUrl of
                Just parsedRuleTree ->
                    ( { model
                        | ruleTree = parsedRuleTree
                        , latestTermVarTypings = updateLatestTermVarTypings model.latestTermVarTypings (getRuleTreeNode parsedRuleTree model.selectedNodeID) True
                        , ruleTreeSuccessful = ruleTreeIsSuccessful parsedRuleTree (getFirstConflictFromRuleTree parsedRuleTree)
                        , displayMessage =
                            if ruleTreeIsSuccessful parsedRuleTree (getFirstConflictFromRuleTree parsedRuleTree) then
                                "Your Proof Tree is complete and correct - Great Job!"

                            else
                                generateDisplayMessageIfRuleTreeIsSuccessfulBesidesContextMissingFreeVar
                                    parsedRuleTree
                                    (getFirstConflictFromRuleTree parsedRuleTree)
                                    |> Maybe.withDefault model.displayMessage
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | displayMessage = "Unexpected parsing error on the prooftree query." }, Cmd.none )

        StartClick ->
            case applyUserInitInputs model of
                Ok ruleTree ->
                    -- if ruleTree's term has variable shadowing, show a friendly error message
                    if RuleTree.variableShadowingIsOccuring (getTermFromRuleTree ruleTree |> Maybe.withDefault (Var "x")) Set.empty then
                        ( { model | displayMessage = varShadowingErrMsg }, Cmd.none )

                    else
                        ( changeViewState SelectRule model |> flushAllInputs, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        GetUrlClick ->
            case applyUserInitInputs model of
                Ok ruleTree ->
                    -- if ruleTree's term has variable shadowing, show a friendly error message
                    if RuleTree.variableShadowingIsOccuring (getTermFromRuleTree ruleTree |> Maybe.withDefault (Var "x")) Set.empty then
                        ( { model | displayMessage = varShadowingErrMsg }, Cmd.none )

                    else
                        ( { model | displayMessage = getUrlWithProoftree model ruleTree }, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        ToggleLatinView ->
            ( { model | viewLatinChar = not model.viewLatinChar }, Cmd.none )

        ToggleHelpText ->
            ( { model
                | viewState =
                    if model.viewState == Help then
                        SelectRule

                    else
                        Help
              }
            , Cmd.none
            )

        NoOperation ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown
            (Json.Decode.map KeyDown
                (Json.Decode.field "key" Json.Decode.string)
            )
        , onUrlChange UrlChanged
        ]



-- PORTS


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    case model.viewState of
        Start ->
            viewHome model

        Help ->
            div [ class "main-application-container" ]
                [ viewHelp
                , viewTypecheckingMenu model
                ]

        _ ->
            div [ class "main-application-container" ]
                [ viewRuleTreeContainer model
                , viewTypecheckingMenu model
                ]


viewTypecheckingMenu : Model -> Html Msg
viewTypecheckingMenu model =
    let
        viewDisplayMessage =
            -- don't show any display message when the help page is active
            if model.viewState == Help then
                text ""

            else
                div [ class "display-message-container" ] [ strong [] [ text model.displayMessage ] ]
    in
    div
        [ class "menu-container" ]
        [ viewTopMenu model
        , viewVarRule model
        , viewApplicationRule model
        , viewAbstractionRule model
        , viewDisplayMessage
        , viewRuleUserInterface model
        ]


viewRuleTreeContainer : Model -> Html Msg
viewRuleTreeContainer model =
    div [ class "ruletree-container" ] [ viewRuleTree model.ruleTree [] model (getFirstConflictFromRuleTree model.ruleTree) ]


viewHome : Model -> Html Msg
viewHome model =
    div [ class "start-page" ]
        [ h1 [ class "start-page__headline" ]
            [ text "STLC λ"
            , span [ style "vertical-align" "super" ] [ text "→" ]
            , text " Typechecker"
            ]
        , div [ class "description-text", style "margin-bottom" "13px" ]
            [ text "Set up the Typing Judgement"
            , span [ class "meta-variable", style "font-weight" "bold" ] [ text "    Γ  ⊢  M  :  τ    " ]
            , text "you would like your Proof Tree to start with!"
            ]
        , div [ class "description-text-block" ]
            [ div [ class "description-text" ]
                [ text "The set of valid Term- and Typevariable Names is defined through the following Regex:"
                , span [ class "code-text" ] [ text " [a-zA-Z][a-zA-Z0-9'_]*" ]
                ]
            , div [ class "description-text" ]
                [ text "Both Terms and Types need all inner Terms and Types to be put into explicit parantheses "
                , text "except for Termvariables and Typevariables."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", style "font-weight" "bold", class "description-text-headline" ] [ text "Γ" ]
            , div [ class "description-text" ]
                [ text "Enter the Context as a set of Typing Assumptions, e.g. "
                , span [ class "code-text" ] [ text " x:a, y:b->(c->a), z:Int->Bool, x:?" ]
                ]
            , div [ class "description-text" ]
                [ text "It is highly recommended to put the Terms Free Variables (FV) with Type '?' in the Context, and no bound Variables!" ]
            , div [ class "description-text" ]
                [ text "The Type '?' is a Wildcard Type and can be understood as \"I don't know the Type yet but I want to make sure to come back to it later!\"" ]
            , div [ class "description-text" ]
                [ text "So it's especially useful for putting Typevariables into a Context for which you know it has to be in there but don't know its Type yet. Example:" ]
            , div [ class "description-text" ]
                [ text "For given Term "
                , span [ class "code-text" ] [ text " (\\z.(\\w.(x w))) u " ]
                , text " with "
                , span [ class "code-text" ] [ text " x " ]
                , text " and "
                , span [ class "code-text" ] [ text " u " ]
                , text " being the FV, it is recommended to enter the Context as follows: "
                , span [ class "code-text" ] [ text " x:?, u:?" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", style "font-weight" "bold", class "description-text-headline" ] [ text "M" ]
            , div [ class "description-text" ]
                [ text "Enter the Lambda Term."
                ]
            , div [ class "description-text" ]
                [ text "Examples:  "
                , span [ class "code-text" ] [ text " x, x y, \\x.y, \\p.(k p), (\\z.(\\w.(x w))) u" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", style "font-weight" "bold", class "description-text-headline" ] [ text "τ" ]
            , div [ class "description-text" ]
                [ text "Enter the Type for Term "
                , span [ class "meta-variable" ] [ text " M." ]
                ]
            , div [ class "description-text" ]
                [ text "It is recommended to leave this field empty and begin the Typing Process in the Application itself!"
                ]
            ]
        , viewNodeInitiationInputs model
        , viewNodeInitiationButtons
        , strong [ style "margin" "30px" ] [ text model.displayMessage ]
        ]


viewHelp : Html Msg
viewHelp =
    div [ class "help-container" ]
        [ div [ class "description-text-block" ]
            [ div [ class "description-text" ]
                [ text "The set of valid Term- and Typevariable Names is defined through the following Regex:"
                , span [ class "code-text" ] [ text " [a-zA-Z][a-zA-Z0-9'_]*" ]
                ]
            , div [ class "description-text" ]
                [ text "Both Terms and Types need all inner Terms and Types to be put into explicit parantheses "
                , text "except for Termvariables and Typevariables."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", class "description-text-headline" ] [ text "Γ" ]
            , div [ class "description-text" ]
                [ text "Enter the corresponding Context as a set of Typing Assumptions"
                ]
            , div [ class "description-text" ]
                [ text "Example:  "
                , span [ class "code-text" ] [ text " x:a, y:b->(c->a), z:Int->Bool, x:?" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", class "description-text-headline" ] [ text "x, M, N" ]
            , div [ class "description-text" ]
                [ text "Enter the corresponding Lambda Term."
                ]
            , div [ class "description-text" ]
                [ text "Examples:  "
                , span [ class "code-text" ] [ text " x, x y, \\x.y, \\p.(k p), (\\z.(\\w.(x w))) u" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "meta-variable", class "description-text-headline" ] [ text "σ, τ" ]
            , div [ class "description-text" ]
                [ text "Enter the corresponding Type(s) for the selected Typing Judgement."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "description-text-headline" ] [ text "💡" ]
            , div [ class "description-text" ]
                [ text "Get a Hint for each kind of input. The given Hint is primarily based on the local and recent information, so it might be just a step closer towards the correct solution,"
                ]
            , div [ class "description-text" ]
                [ text "or it might even be completely wrong if there are mistakes in the Proof Tree."
                ]
            , div [ class "description-text" ]
                [ text "The Hint function can also be used for propagating changes through the Proof Tree."
                ]
            , div [ class "description-text" ]
                [ text "If you have changed a Type in a certain node, then using the Hint function on adjacent nodes can help to quickly get your changes applied on other places."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "description-text-headline" ] [ text "💊" ]
            , div [ class "description-text" ]
                [ text "This takes all the stuff that is within your currently selected Tree Node and puts it into the Input Fields."
                ]
            , div [ class "description-text" ]
                [ text "This can be helpful for inner Tree Nodes to make corrections,"
                ]
            , div [ class "description-text" ]
                [ text "or for outer Tree Nodes to save some time if you already feel confident on these steps."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "description-text-headline" ] [ text "🧹" ]
            , div [ class "description-text" ]
                [ text "The broom sweeps all inputs."
                ]
            ]
        , div [ class "description-text-block" ]
            [ h3 [ class "description-text-headline" ] [ text "♻️" ]
            , div [ class "description-text" ]
                [ text "This propagates your most recent changes on all of the Typings for Term Variables through the whole Proof Tree."
                ]
            , div [ class "description-text" ]
                [ text "So whenever you change a Type that belongs to a Term Variable, you have the option to automatically propagate that change globally."
                ]
            , div [ class "description-text" ]
                [ text "Does NOT include changes that are just written in input fields but have not been applied yet."
                ]
            , div [ class "description-text" ]
                [ text "Works on Contexts, Typing Judgements that have a Variable as their Term, as well as on the Typing of Variables in Abstractions."
                ]
            ]
        , div [ style "margin-top" "42px" ]
            [ button
                [ class "menu__top-button"
                , onClick ToggleHelpText
                , style "font-family" "Monaco, monospace"
                , style "font-size" "130%"
                ]
                [ text " Return to Proof Tree " ]
            ]
        ]


viewTopMenu : Model -> Html Msg
viewTopMenu model =
    let
        ( toggleLatinButtonLabel, toggleLatinButtonTooltip ) =
            if model.viewLatinChar then
                ( "τ", "Switch to the Greek representation of Type Variables" )

            else
                ( "t", "Switch to the Latin representation of Type Variables" )

        ( helpButtonLabel, helpButtonTooltip ) =
            if model.viewState == Help then
                ( "Return", "Return to Proof Tree" )

            else
                ( "?", "Show the Help Page (You can return to the Proof Tree and continue where you have stopped)" )
    in
    div [ class "menu__top-button-container" ]
        [ button
            [ onClick ToggleLatinView, class "menu__top-button", title toggleLatinButtonTooltip ]
            [ text toggleLatinButtonLabel ]
        , button [ class "menu__top-button", onClick ToggleHelpText, title helpButtonTooltip, style "font-family" "Monaco, monospace" ] [ text helpButtonLabel ]
        , button
            [ onClick <| ChangeViewState Start
            , class "menu__top-button"
            , style "font-size" "85%"
            , style "padding-left" "5px"
            , style "padding-right" "5px"
            , title "Go to the Start Page"
            ]
            [ text "🏠" ]
        ]
