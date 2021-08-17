port module Main exposing (..)

import Browser
import Browser.Events
import Browser.Navigation exposing (pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Regex
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus as STLC exposing (..)
import Url
import Url.Builder
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
    ( { menuState = SelectRule
      , ruleTree = getRuleTreeFromUrlQuery locationHref
      , selectedNodeId = []
      , gammaInput = ""
      , xInput = ""
      , mInput = ""
      , nInput = ""
      , sigmaInput = ""
      , tauInput = ""
      , displayMessage = "Welcome to the interactive STLC Typechecker. Start by selecting an inference rule right above this message!"
      }
    , Cmd.none
    )


getUrlWithProoftree : RuleTree -> String
getUrlWithProoftree ruleTree =
    Url.Builder.crossOrigin "https://www.whatever.de" [] [ Url.Builder.string "prooftree" (STLC.encodeRuleTreeAsString ruleTree) ]


getRuleTreeFromUrlQuery : String -> RuleTree
getRuleTreeFromUrlQuery urlAsString =
    parseRuleTree <| showUrlQuery "prooftree" <| getValidUrlFromLocalPath urlAsString


getValidUrlFromLocalPath : String -> String
getValidUrlFromLocalPath currentPath =
    String.replace "file:///D:/0-Drive/Backups/vscode%20project%20-%20typechecker/typechecker.html" "https://www.whatever.de/" currentPath


fixUrl : String -> String
fixUrl =
    case Regex.fromString "^file://" of
        Nothing ->
            identity

        Just regex ->
            Regex.replace regex (\_ -> "https://blubb")


voidUrl : Url.Url
voidUrl =
    Url.Url Url.Http "" Nothing "" Nothing Nothing


localPath =
    "file:///D:/0-Drive/Backups/vscode%20project%20-%20typechecker/typechecker.html?prooftree="


showUrlQuery : String -> String -> String
showUrlQuery query urlAsString =
    Maybe.withDefault "" <| Maybe.withDefault Nothing <| (Url.Parser.parse <| Url.Parser.query <| Query.string query) <| Maybe.withDefault voidUrl (Url.fromString urlAsString)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    {- let
           _ =
               Debug.log "crossOrigin" <| getUrlWithProoftree model.ruleTree

           _ =
               Debug.log "Currently selected RuleTree" <| getRuleTreeNode model.ruleTree model.selectedNodeId
       in
    -}
    {- let
           model =
               { modelCurrent | displayMessage = "" }
       in
    -}
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
            ( model, Cmd.none )

        TransformInput ->
            ( { model
                | gammaInput = UserInput.stringToSpecialCharacterRepresantation model.gammaInput
                , mInput = UserInput.stringToSpecialCharacterRepresantation model.mInput
                , nInput = UserInput.stringToSpecialCharacterRepresantation model.nInput
                , sigmaInput = UserInput.stringToSpecialCharacterRepresantation model.sigmaInput
                , tauInput = UserInput.stringToSpecialCharacterRepresantation model.tauInput
              }
            , Cmd.none
            )

        FillAllInputs ->
            ( UserInput.fillAllInputsFromRuleTree (getSelectedRuleTreeNode model) model, Cmd.none )

        Submit ->
            ( { model | ruleTree = UserInput.updateSelectedRuleTreeNode model }, pushUrl <| localPath ++ encodeRuleTreeAsString (UserInput.updateSelectedRuleTreeNode model) )

        SelectTreeNode nodeId ->
            ( fillAllInputsFromRuleTree (getRuleTreeNode model.ruleTree nodeId) { model | selectedNodeId = nodeId }
                |> adjustMenuStateToSelectedRuleTree
            , Cmd.none
            )

        ResetTreeNode nodeId ->
            ( { model | ruleTree = resetRuleTreeNode model.ruleTree nodeId }
                |> adjustMenuStateToSelectedRuleTree
            , pushUrl <| localPath ++ encodeRuleTreeAsString (resetRuleTreeNode model.ruleTree nodeId)
            )

        ChangeState newState ->
            ( { model
                | menuState = newState
                , displayMessage =
                    if List.member newState [ VarRule, AbsRule, AppRule ] then
                        "Fill out the input fields and hit 'Apply'!"

                    else
                        ""
              }
            , Cmd.none
            )

        KeyDown key ->
            ( if key == "ArrowUp" then
                { model | selectedNodeId = STLC.getNodeIdForArrowUpKeyEvent model.ruleTree model.selectedNodeId }

              else if key == "ArrowDown" then
                { model | selectedNodeId = STLC.getNodeIdForArrowDownKeyEvent model.ruleTree model.selectedNodeId }

              else if key == "ArrowLeft" then
                { model | selectedNodeId = STLC.getNodeIdForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeId True }

              else if key == "ArrowRight" then
                { model | selectedNodeId = STLC.getNodeIdForArrowLeftOrRightKeyEvent model.ruleTree model.selectedNodeId False }

              else
                model
            , Cmd.none
            )

        UrlChanged str ->
            {- let
                   _ =
                       Debug.log "UrlChangedMsg str: " str
               in
            -}
            ( model, Cmd.none )

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
    div [ class "flex-container" ]
        [ viewLeft model
        , viewRight model
        ]


viewRight : Model -> Html Msg
viewRight model =
    let
        ruleIsSelected =
            List.member model.menuState [ VarRule, AbsRule, AppRule ]

        viewRuleUserInterfaceOnSelection =
            if ruleIsSelected then
                viewRuleUserInterface model

            else
                text ""

        viewDisplayMessage =
            div [ class "display-message-container" ] [ strong [] [ text model.displayMessage ] ]
    in
    div
        [ class "menu" ]
        [ viewVarRule model
        , viewApplicationRule model
        , viewAbstractionRule model
        , viewDisplayMessage
        , viewRuleUserInterfaceOnSelection
        ]


viewLeft : Model -> Html Msg
viewLeft model =
    div [ class "tree" ] [ viewRuleTree model.ruleTree [] model (getFirstConflictFromRuleTree model.ruleTree) ]
