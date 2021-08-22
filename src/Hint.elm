module Hint exposing (..)

import Array
import Dict
import Json.Decode exposing (dict)
import Set exposing (Set)
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)
import UserInput exposing (charToTypingRepresentation, fillGammaInputFromRuleTree, fillMInputFromRuleTree, fillNInputFromRuleTree, fillXInputFromRuleTree, validVarAndTypeVarInputs)



{- type InputField
   = GammaInput
   | XInput
   | MInput
   | NInput
   | SigmaInput
   | TauInput
-}


getHint : InputKind -> Model -> Model
getHint inputKind model =
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
        ( RVar context term _ _, VarRule ) ->
            case inputKind of
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

        ( RAbs _ term _ nextRuleTree, AbsRule ) ->
            case inputKind of
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

                TauInput ->
                    case ( term, getUnusedTypeVar 1 ) of
                        ( Abs _ _, Just unusedTypeVar ) ->
                            { model
                                | tauInput =
                                    getTermTypeFromRuleTree nextRuleTree
                                        |> Maybe.map showType
                                        |> Maybe.withDefault (String.fromChar unusedTypeVar)
                            }

                        ( _, Nothing ) ->
                            tooManyTypeVarInUse

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( RApp _ term _ nextRuleTree1 nextRuleTree2, AppRule ) ->
            case inputKind of
                GammaInput ->
                    fillGammaInputFromRuleTree selectedRuleTree model

                MInput ->
                    case term of
                        App _ _ ->
                            fillMInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                NInput ->
                    case term of
                        App _ _ ->
                            fillNInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    let
                        typeFromRuleTreeOrUnusedTypeVar =
                            case Debug.log "unusedTypeVar" (getUnusedTypeVar 0) of
                                Just unusedTypeVar ->
                                    { model
                                        | sigmaInput =
                                            getTermTypeFromRuleTree nextRuleTree2
                                                |> Maybe.map showType
                                                |> Maybe.withDefault (String.fromChar unusedTypeVar)
                                    }

                                Nothing ->
                                    tooManyTypeVarInUse

                        -- if term N is a certain variable x, and x has a type in this or the parents context, then we hint sigma to be that type
                        termNOfFormVar =
                            case term of
                                App _ (Var var) ->
                                    getTypeForVarFromLocalOrParentNode var model.selectedNodeId selectedRuleTree model
                                        |> (\typ ->
                                                case typ of
                                                    Just sigmaTyp ->
                                                        Just { model | sigmaInput = showType sigmaTyp }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing

                        -- if term M is a certain variable x, and x has an Arrow type in this or the parents context, then we hint sigma to be the left type of that Arrow type
                        termMOfFormVar =
                            case term of
                                App (Var var) _ ->
                                    getTypeForVarFromLocalOrParentNode var model.selectedNodeId selectedRuleTree model
                                        |> (\typ ->
                                                case typ of
                                                    Just (Arrow sigmaTyp _) ->
                                                        Just { model | sigmaInput = showType sigmaTyp }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing

                        -- if term M is a certain abstraction (\x.M), and x has a type in this or the parents context, then we hint sigma to be that type
                        termMOfFormAbs =
                            case term of
                                App (Abs var _) _ ->
                                    getTypeForVarFromLocalOrParentNode var model.selectedNodeId selectedRuleTree model
                                        |> (\typ ->
                                                case typ of
                                                    Just sigmaTyp ->
                                                        Just { model | sigmaInput = showType sigmaTyp }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing
                    in
                    -- trying to find a type for sigma in the local or parents context, otherweise we use an unused typeVar or term and rule doesnt match up
                    case ( termNOfFormVar, termMOfFormAbs, ( termMOfFormVar, term ) ) of
                        ( Just newModel, _, ( _, _ ) ) ->
                            newModel

                        ( _, Just newModel, ( _, _ ) ) ->
                            newModel

                        ( _, _, ( Just newModel, _ ) ) ->
                            newModel

                        ( _, _, ( _, App _ _ ) ) ->
                            typeFromRuleTreeOrUnusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                TauInput ->
                    case ( term, getUnusedTypeVar 1 ) of
                        ( Abs _ _, Just unusedTypeVar ) ->
                            { model
                                | tauInput =
                                    getSigmaTypeFromAbsRuleTree nextRuleTree1
                                        |> Maybe.map showType
                                        |> Maybe.withDefault (String.fromChar unusedTypeVar)
                            }

                        ( _, Nothing ) ->
                            tooManyTypeVarInUse

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( ruleTree, SelectRule ) ->
            case ( inputKind, getTermFromRuleTree ruleTree ) of
                ( RuleSelection, Just (Var _) ) ->
                    changeState VarRule model

                ( RuleSelection, Just (Abs _ _) ) ->
                    changeState AbsRule model

                ( RuleSelection, Just (App _ _) ) ->
                    changeState AppRule model

                ( RuleSelection, Nothing ) ->
                    { model | displayMessage = "Could not determine the corresponding inference rule. Try to select a valid tree node!" }

                _ ->
                    { model | displayMessage = "Unexpected program state. Try to continue or reload the page!" }

        _ ->
            { model
                | displayMessage =
                    "The currently selected inference rule does not correspond to the currently selected node. Change (at least) one of these!"
                        ++ " (Changing the inference rule requires to click on 'Apply')"
            }


{-| Gets the type for `var` from the nodes context if available, otherwise from the parents context.
-}
getTypeForVarFromLocalOrParentNode : Var -> List Int -> RuleTree -> Model -> Maybe SType
getTypeForVarFromLocalOrParentNode var nodeId ruleTree model =
    let
        getTypeFromRuleTreeContext ruleTree1 var1 =
            getContextFromRuleTree ruleTree1 |> getTypeFromContext var1
    in
    case getTypeFromRuleTreeContext ruleTree var of
        Just typ ->
            Just typ

        Nothing ->
            case nodeId of
                _ :: parentNodeId ->
                    getTypeFromRuleTreeContext (getRuleTreeNode model.ruleTree parentNodeId) var

                _ ->
                    Nothing


setOfAllTypeVariables : Set Var
setOfAllTypeVariables =
    List.map charToTypingRepresentation validVarAndTypeVarInputs |> Set.fromList


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
