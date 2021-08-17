module Hint exposing (..)

import Array
import Dict
import Json.Decode exposing (dict)
import Set exposing (Set)
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)
import UserInput exposing (fillGammaInputFromRuleTree, fillMInputFromRuleTree, fillXInputFromRuleTree, sigmaInput)



{- type InputField
   = GammaInput
   | XInput
   | MInput
   | NInput
   | SigmaInput
   | TauInput
-}


getHint : InputField -> Model -> Model
getHint inputField model =
    let
        selectedRuleTree =
            getSelectedRuleTreeNode model

        termAndRuleDoNotMatchUp =
            { model
                | displayMessage =
                    "The term and inference rule of this node do not match up. Change (at least) one of these!"
                        ++ " (Changing the inference rule requires to click on 'Apply')"
            }

        tooManyTypeVarInUse =
            { model | displayMessage = "Too many type variables in use. Try freeing some up!" }

        getUnusedTypeVar index =
            getUnusedTypeVariableFromRuleTree model.ruleTree index
    in
    case ( selectedRuleTree, model.menuState ) of
        ( RVar context term typ _, VarRule ) ->
            case inputField of
                GammaInput ->
                    fillGammaInputFromRuleTree selectedRuleTree model

                XInput ->
                    case term of
                        Var _ ->
                            fillXInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    case ( term, getUnusedTypeVar 0 ) of
                        ( Var var, Just unusedTypeVar ) ->
                            { model
                                | sigmaInput =
                                    getTypeFromContext var context
                                        |> Maybe.map showType
                                        |> Maybe.withDefault (String.fromChar unusedTypeVar)
                            }

                        ( _, Nothing ) ->
                            tooManyTypeVarInUse

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( RAbs context term typ nextRuleTree, AbsRule ) ->
            case inputField of
                GammaInput ->
                    fillGammaInputFromRuleTree selectedRuleTree model

                XInput ->
                    case term of
                        Abs _ _ ->
                            fillXInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                MInput ->
                    case term of
                        Abs _ _ ->
                            fillMInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    case ( term, getUnusedTypeVar 0 ) of
                        ( Abs var _, Just unusedTypeVar ) ->
                            { model
                                | sigmaInput =
                                    getTypeFromContext var (getContextFromRuleTree nextRuleTree)
                                        |> Maybe.map showType
                                        |> Maybe.withDefault (String.fromChar unusedTypeVar)
                            }

                        ( _, Nothing ) ->
                            tooManyTypeVarInUse

                        _ ->
                            termAndRuleDoNotMatchUp

                -- TODO
                TauInput ->
                    case ( term, getUnusedTypeVar 1 ) of
                        ( Abs _ _, Just unusedTypeVar ) ->
                            { model
                                | tauInput =
                                    getTermTypeFromRuleTree nextRuleTree
                                        |> showType
                            }

                        ( _, Nothing ) ->
                            tooManyTypeVarInUse

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( RApp context term typ ruleTree1 ruleTree2, AppRule ) ->
            model

        ( Hole, SelectRule ) ->
            model

        _ ->
            model


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


getUnusedTypeVariableFromRuleTree : RuleTree -> Int -> Maybe Char
getUnusedTypeVariableFromRuleTree ruleTree index =
    getUsedTypeVariables ruleTree
        |> Set.diff setOfAllTypeVariables
        |> Set.toList
        |> Array.fromList
        |> Array.get index
