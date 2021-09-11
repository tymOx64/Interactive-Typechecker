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
import RuleTree exposing (encodeRuleTreeAsString, generateDisplayMessageIfRuleTreeIsSuccessfulBesidesContextMissingFreeVar, getFirstConflictFromRuleTree, getNodeIdForArrowDownKeyEvent, getNodeIdForArrowLeftOrRightKeyEvent, getNodeIdForArrowUpKeyEvent, resetRuleTreeNode, ruleTreeIsSuccessful, viewAbstractionRule, viewApplicationRule, viewRuleTree, viewVarRule)
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
    ( { menuState =
            if noProoftreeQueryGiven then
                CreateStartingNode

            else
                SelectRule
      , ruleTree = initialRuleTree
      , selectedNodeId = []
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

                -- for the init screen, don't show any display message initially
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
    Maybe.andThen parseRuleTree <| Debug.log "showUrlQuery" <| getUrlQuery "prooftree" <| Debug.log "fixUrl" <| fixLocalUrl urlAsString


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
            5

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

        Hint inputField ->
            ( getHint inputField model, Cmd.none )

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
                    getRuleTreeNode ruleTreeWithLatestTermVarTypings model.selectedNodeId
            in
            ( model, pushUrl <| getUrlWithProoftree model <| passChangesThroughRuleTree currentlySelectedRuleTree model.selectedNodeId ruleTreeWithLatestTermVarTypings )

        Apply ->
            case UserInput.applyUserInputsToSelectedRuleTreeNode model of
                Ok ruleTree ->
                    ( flushAllInputs model, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        SelectTreeNode nodeId ->
            ( adjustMenuStateToSelectedRuleTree { model | selectedNodeId = nodeId }, Cmd.none )

        ResetTreeNode nodeId ->
            ( adjustMenuStateToSelectedRuleTree model
            , pushUrl <| getUrlWithProoftree model (resetRuleTreeNode model.ruleTree nodeId)
            )

        ChangeState newState ->
            ( changeState newState model, Cmd.none )

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
                        pushUrl <| getUrlWithProoftree model (resetRuleTreeNode model.ruleTree model.selectedNodeId)

                    else
                        Cmd.none
            in
            ( if key == "ArrowUp" then
                { model | selectedNodeId = getNodeIdForArrowUpKeyEvent model.ruleTree model.selectedNodeId }
                    |> adjustMenuStateToSelectedRuleTree

              else if key == "ArrowDown" then
                { model | selectedNodeId = getNodeIdForArrowDownKeyEvent model.ruleTree model.selectedNodeId }
                    |> adjustMenuStateToSelectedRuleTree

              else if key == "ArrowLeft" then
                { model | selectedNodeId = getNodeIdForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeId True }
                    |> adjustMenuStateToSelectedRuleTree

              else if key == "ArrowRight" then
                { model | selectedNodeId = getNodeIdForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeId False }
                    |> adjustMenuStateToSelectedRuleTree

              else if key == "1" then
                changeState VarRule model

              else if key == "2" then
                changeState AppRule model

              else if key == "3" then
                changeState AbsRule model

              else if key == "Enter" then
                -- the actual application happens through the cmd (see "let in" above)
                case UserInput.applyUserInputsToSelectedRuleTreeNode model of
                    Ok _ ->
                        adjustMenuStateToSelectedRuleTree model
                            |> flushAllInputs

                    Err err ->
                        { model | displayMessage = err }

              else if key == "Delete" then
                -- the actual deletion happens through the cmd (see "let in" above)
                adjustMenuStateToSelectedRuleTree model

              else
                model
            , cmd
            )

        UrlChanged newUrl ->
            case getRuleTreeFromUrlQuery newUrl of
                Just parsedRuleTree ->
                    ( { model
                        | ruleTree = parsedRuleTree
                        , latestTermVarTypings = updateLatestTermVarTypings model.latestTermVarTypings (getRuleTreeNode parsedRuleTree model.selectedNodeId) True
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
                    ( { model | ruleTree = model.ruleTree, displayMessage = "Unexpected parsing error on the prooftree query." }, Cmd.none )

        Start ->
            case applyUserInitInputs model of
                Ok ruleTree ->
                    -- if ruleTree's term has variable shadowing, show a friendly error message
                    if RuleTree.variableShadowingIsOccuring (getTermFromRuleTree ruleTree |> Maybe.withDefault (Var 'x')) Set.empty then
                        ( { model | displayMessage = varShadowingErrMsg }, Cmd.none )

                    else
                        ( changeState SelectRule model |> flushAllInputs, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        GetUrl ->
            case applyUserInitInputs model of
                Ok ruleTree ->
                    -- if ruleTree's term has variable shadowing, show a friendly error message
                    if RuleTree.variableShadowingIsOccuring (getTermFromRuleTree ruleTree |> Maybe.withDefault (Var 'x')) Set.empty then
                        ( { model | displayMessage = varShadowingErrMsg }, Cmd.none )

                    else
                        ( { model | displayMessage = getUrlWithProoftree model ruleTree }, pushUrl <| getUrlWithProoftree model ruleTree )

                Err err ->
                    ( { model | displayMessage = err }, Cmd.none )

        ToggleLatinView ->
            ( { model | viewLatinChar = not model.viewLatinChar }, Cmd.none )

        StartPage ->
            ( { model | menuState = CreateStartingNode, displayMessage = "" }, Cmd.none )

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


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    case model.menuState of
        CreateStartingNode ->
            viewInitStartingNode model

        _ ->
            div [ class "main-application-container" ]
                [ viewLeft model
                , viewRight model
                ]


viewRight : Model -> Html Msg
viewRight model =
    let
        viewDisplayMessage =
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


viewLeft : Model -> Html Msg
viewLeft model =
    div [ class "ruletree-container" ] [ viewRuleTree model.ruleTree [] model (getFirstConflictFromRuleTree model.ruleTree) ]


viewInitStartingNode : Model -> Html Msg
viewInitStartingNode model =
    div [ class "init-starting-node" ]
        [ h1 [ class "init-starting-node__headline" ]
            [ text "STLC λ"
            , span [ style "vertical-align" "super" ] [ text "→" ]
            , text " Typechecker"
            ]
        , div [ class "description-text", style "margin-bottom" "13px" ]
            [ text "Set up the Typing Judgement"
            , span [ class "meta-variable" ] [ text "    Γ  ⊢  M  :  τ    " ]
            , text "you would like your Proof Tree to start with!"
            ]
        , div [ class "description-text-block" ]
            [ div [ class "description-text" ]
                [ text "The Set of valid Typevariable Names is defined through the following Regex:"
                , span [ class "code-text" ] [ text " [a-zA-Z][a-zA-Z0-9'_]*" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ div [ class "description-text" ]
                [ span [ class "meta-variable" ] [ text "Γ " ]
                , text ":  Enter the Context as a Set of Typing Assumptions, e.g. "
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
            [ div [ class "description-text" ]
                [ span [ class "meta-variable" ] [ text "M " ]
                , text ":  Enter the Lambda Term with all inner Terms (except Variables) put in parantheses. Examples:"
                ]
            , div [ class "description-text" ]
                [ span [ class "code-text" ] [ text " x, x y, \\x.y, \\p.(k p), (\\z.(\\w.(x w))) u" ]
                ]
            ]
        , div [ class "description-text-block" ]
            [ div [ class "description-text" ]
                [ span [ class "meta-variable" ] [ text "τ " ]
                , text ":  Enter the Type for Term "
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


viewTopMenu : Model -> Html Msg
viewTopMenu model =
    let
        ( toggleLatinButtonLabel, toggleLatinButtonTooltip ) =
            if model.viewLatinChar then
                ( "τ", "Switch to the Greek representation of Type Variables" )

            else
                ( "t", "Switch to the Latin representation of Type Variables" )
    in
    div [ class "menu__top-button-container" ]
        [ button
            [ onClick ToggleLatinView, class "menu__top-button", title toggleLatinButtonTooltip ]
            [ text toggleLatinButtonLabel ]
        , button
            [ onClick StartPage
            , class "menu__top-button"
            , style "font-size" "85%"
            , style "padding-left" "5px"
            , style "padding-right" "5px"
            , title "Go to the Start Page"
            ]
            [ text "🏠" ]
        ]
