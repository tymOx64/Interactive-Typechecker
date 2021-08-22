module Hint exposing (..)

import Array
import Dict
import Json.Decode exposing (dict)
import Set exposing (Set)
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)
import Tuple exposing (first)
import UserInput exposing (charToTypingRepresentation, fillGammaInputFromRuleTree, fillMInputFromRuleTree, fillNInputFromRuleTree, fillXInputFromRuleTree, validVarAndTypeVarInputs)


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

        ( RApp _ thisTerm thisType nextRuleTree1 nextRuleTree2, AppRule ) ->
            case inputKind of
                GammaInput ->
                    fillGammaInputFromRuleTree selectedRuleTree model

                MInput ->
                    case thisTerm of
                        App _ _ ->
                            fillMInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                NInput ->
                    case thisTerm of
                        App _ _ ->
                            fillNInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    let
                        -- nextRuleTree2 has a typing of form N : sigma' and we hint for that sigma' before an unused typeVar
                        typeFromNextRuleTree2_OrUnusedTypeVar =
                            case getUnusedTypeVar 0 of
                                Just unusedTypeVar ->
                                    { model
                                        | sigmaInput =
                                            getTermTypeFromRuleTree nextRuleTree2
                                                |> Maybe.map showType
                                                |> Maybe.withDefault (String.fromChar unusedTypeVar)
                                    }

                                Nothing ->
                                    tooManyTypeVarInUse

                        -- if term N is a certain variable x, traverse the full ruleTree to the first context that contains the type for x
                        termNOfFormVar =
                            case thisTerm of
                                App _ (Var var) ->
                                    getTypeForVarFromFirstContextMatch var model.ruleTree
                                        |> (\typ ->
                                                case typ of
                                                    Just sigmaType ->
                                                        Just { model | sigmaInput = showType sigmaType }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing

                        -- if term M is a certain variable x, traverse the full ruleTree to the first context that contains the type for x
                        termMOfFormVar =
                            case thisTerm of
                                App (Var var) _ ->
                                    getTypeForVarFromFirstContextMatch var model.ruleTree
                                        |> (\typ ->
                                                case typ of
                                                    Just (Arrow sigmaType _) ->
                                                        Just { model | sigmaInput = showType sigmaType }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing

                        -- if term M is a certain abstraction (\x.T), traverse the full ruleTree to the first context that contains the type for x
                        termMOfFormAbs =
                            case thisTerm of
                                App (Abs var _) _ ->
                                    getTypeForVarFromFirstContextMatch var model.ruleTree
                                        |> (\typ ->
                                                case typ of
                                                    Just sigmaType ->
                                                        Just { model | sigmaInput = showType sigmaType }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing
                    in
                    -- trying to find a type for sigma all other contexts, otherwise we use an unused typeVar or term and rule doesnt match up
                    -- using nested tuples here just because Elm v0.19.1 does not allow tuples with more than 3 values
                    case ( termNOfFormVar, termMOfFormAbs, ( termMOfFormVar, thisTerm ) ) of
                        ( Just newModel, _, ( _, _ ) ) ->
                            newModel

                        ( _, Just newModel, ( _, _ ) ) ->
                            newModel

                        ( _, _, ( Just newModel, _ ) ) ->
                            newModel

                        ( _, _, ( _, App _ _ ) ) ->
                            typeFromNextRuleTree2_OrUnusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                TauInput ->
                    let
                        typeFromThisRuleTreeOrUnusedTypeVar =
                            case getUnusedTypeVar 1 of
                                Just unusedTypeVar ->
                                    { model
                                        | tauInput =
                                            if thisType /= Untyped then
                                                showType thisType

                                            else
                                                String.fromChar unusedTypeVar
                                    }

                                Nothing ->
                                    tooManyTypeVarInUse

                        -- if term M is a certain variable x, traverse the full ruleTree to the first context that contains the type for x
                        -- and if x is an Arrow type then we hint sigma to be the right type of that Arrow type
                        termMOfFormVar =
                            case thisTerm of
                                App (Var var) _ ->
                                    getTypeForVarFromFirstContextMatch var model.ruleTree
                                        |> (\typ ->
                                                case typ of
                                                    Just (Arrow _ tauType) ->
                                                        Just { model | sigmaInput = showType tauType }

                                                    _ ->
                                                        Nothing
                                           )

                                _ ->
                                    Nothing

                        -- if we find them terms type in ruleTree1 to be an Arrow type, we take the right type (tauType) of that
                        tauTypeFromRuleTree1 =
                            getTermTypeFromRuleTree nextRuleTree1
                                |> (\typ ->
                                        case typ of
                                            Just (Arrow _ tauType) ->
                                                Just { model | sigmaInput = showType tauType }

                                            _ ->
                                                Nothing
                                   )
                    in
                    case ( termMOfFormVar, tauTypeFromRuleTree1, thisTerm ) of
                        ( Just newModel, _, _ ) ->
                            newModel

                        ( _, Just newModel, _ ) ->
                            newModel

                        ( _, _, App _ _ ) ->
                            typeFromThisRuleTreeOrUnusedTypeVar

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


{-| Traverses `ruleTree` and returns the type for `var` from the first context if available, otherwise from the parents context.
-}
getTypeForVarFromFirstContextMatch : Var -> RuleTree -> Maybe SType
getTypeForVarFromFirstContextMatch var ruleTree =
    let
        getTypeFromRuleTreeContext ruleTree_ =
            getContextFromRuleTree ruleTree_ |> getTypeFromContext var

        returnSecondIfItsAJust first second =
            case second of
                Just _ ->
                    second

                _ ->
                    first
    in
    case ruleTree of
        RVar _ _ _ _ ->
            getTypeFromRuleTreeContext ruleTree

        RAbs _ _ _ nextRuleTree ->
            getTypeFromRuleTreeContext ruleTree
                |> returnSecondIfItsAJust (getTypeForVarFromFirstContextMatch var nextRuleTree)

        RApp _ _ _ nextRuleTree1 nextRuleTree2 ->
            getTypeFromRuleTreeContext ruleTree
                |> returnSecondIfItsAJust (getTypeForVarFromFirstContextMatch var nextRuleTree1)
                |> returnSecondIfItsAJust (getTypeForVarFromFirstContextMatch var nextRuleTree2)

        Hole ->
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
