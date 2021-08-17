module Hint exposing (..)

import Array
import Dict
import Html exposing (var)
import Json.Decode exposing (dict)
import Set exposing (Set)
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)
import UserInput exposing (fillGammaInputFromRuleTree, fillXInputFromRuleTree, sigmaInput)



{- type InputField
   = GammaInput
   | XInput
   | MInput
   | NInput
   | SigmaInput
   | TauInput
-}
{- getHint : InputField -> Model -> Model
   getHint inputField model =
       let
           selectedRuleTree =
               STLC.getSelectedRuleTreeNode model

           modelAsTermAndRuleDoNotMatchUp =
               { model | displayMessage =
                   "The term and inference rule of this node do not match up. Change (at least) one of these!
                   (Changing the inference rule requires to click on 'Apply')"
               }
       in
       case ( selectedRuleTree, model.menuState ) of
           ( ( RVar context term typ ), MenuState.VarRule ) ->
               case InputField of
                   GammaInput ->
                       fillGammaInputFromRuleTree model
                   XInput ->
                       case term of
                           (Var _) ->
                               fillXInputFromRuleTree model
                           _ ->
                               modelAsTermAndRuleDoNotMatchUp
                   SigmaInput ->
                       case term of
                           (Var var) ->
                               case getTypeFromContext var context of
                                   Nothing ->
                                       { model | displayMessage = "" }
                               { model | sigmaInput = getTypeFromContext var context |> showType }
                           _ ->
                               modelAsTermAndRuleDoNotMatchUp
           ( ( RAbs context term typ ruleTree ), MenuState.AbsRule ) ->
               5

           ( ( RApp context term typ ruleTree1 ruleTree2, MenuState.AppRule ) ->
               5

           ( Hole, MenuState.SelectRule ->
               5
-}


setOfAllTypeVariables : Set Var
setOfAllTypeVariables =
    Set.fromList [ 'α', 'β', 'γ', 'δ', 'ε', 'ζ', 'η', 'θ', 'ι', 'κ', 'λ', 'μ', 'ν', 'ξ', 'ο' ]


getUsedTypeVariables : RuleTree -> Set Char
getUsedTypeVariables ruleTree =
    let
        typeVariablesFromTypeToSet typ =
            case typ of
                BasicType var ->
                    Set.singleton var

                Arrow left right ->
                    Set.union (typeVariablesFromTypeToSet left) (typeVariablesFromTypeToSet right)

                Untyped ->
                    Set.empty

        typeVariablesFromContextToSet (Context dict) =
            Dict.foldl (\_ typ set -> Set.union set <| typeVariablesFromTypeToSet typ) Set.empty dict
    in
    case ruleTree of
        RVar context _ typ _ ->
            Set.union (typeVariablesFromContextToSet context) (typeVariablesFromTypeToSet typ)

        RAbs context _ typ ruleTree1 ->
            Set.union (typeVariablesFromContextToSet context) (typeVariablesFromTypeToSet typ)
                |> Set.union (getUsedTypeVariables ruleTree1)

        RApp context _ typ ruleTree1 ruleTree2 ->
            Set.union (typeVariablesFromContextToSet context) (typeVariablesFromTypeToSet typ)
                |> Set.union (getUsedTypeVariables ruleTree1)
                |> Set.union (getUsedTypeVariables ruleTree2)

        Hole ->
            Set.empty


getUnusedTypeVariable : RuleTree -> Int -> Maybe Char
getUnusedTypeVariable ruleTree index =
    getUsedTypeVariables ruleTree
        |> Set.diff setOfAllTypeVariables
        |> Set.toList
        |> Array.fromList
        |> Array.get index
