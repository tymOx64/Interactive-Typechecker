module Hint exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode exposing (dict)
import Set exposing (Set)
import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)
import Tuple exposing (first)
import UserInput exposing (fillMInputFromRuleTree, fillNInputFromRuleTree, fillXInputFromRuleTree, lowerCaseLatinAlphabet)


{-| Gives a hint based on some limited information from the `RuleTree`. Hints may be _incorrect_.
This is just to a certain extend a tool for finding new correct steps and to some other extend a tool
that allows a user to autofill an input field or to proceed some changes through the RuleTree after
updating a certain node.
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

        currentContextDict =
            case getContextFromRuleTree selectedRuleTree of
                Context dict ->
                    dict

        -- brings the current context up-to-date by model.latestTypings
        gammaHint =
            Dict.foldl
                (\var typ newDict -> Dict.insert var (Dict.get var model.latestTypings |> Maybe.withDefault typ) newDict)
                Dict.empty
                currentContextDict
                |> Context
                |> showContext
                |> (\hintedContext -> { model | gammaInput = hintedContext })
    in
    case ( selectedRuleTree, model.menuState ) of
        ( RVar _ thisTerm thisType _, VarRule ) ->
            case inputKind of
                GammaInput ->
                    gammaHint

                XInput ->
                    case thisTerm of
                        Var _ ->
                            fillXInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    let
                        latestTypingForX =
                            case thisTerm of
                                Var var ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestXType -> Just { model | sigmaInput = showType latestXType })

                                _ ->
                                    Nothing

                        typeFromThisNode =
                            case thisType of
                                Untyped ->
                                    Nothing

                                _ ->
                                    Just { model | sigmaInput = showType thisType }

                        unusedTypeVar =
                            case getUnusedTypeVar 0 of
                                Just newTypeVar ->
                                    { model | sigmaInput = newTypeVar }

                                Nothing ->
                                    tooManyTypeVarInUse
                    in
                    case ( thisTerm, latestTypingForX, typeFromThisNode ) of
                        ( Var _, Just newModel, _ ) ->
                            newModel

                        ( Var _, _, Just newModel ) ->
                            newModel

                        ( Var _, _, _ ) ->
                            unusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( RAbs _ thisTerm _ _, AbsRule ) ->
            case inputKind of
                GammaInput ->
                    gammaHint

                XInput ->
                    case thisTerm of
                        Abs _ _ ->
                            fillXInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                MInput ->
                    case thisTerm of
                        Abs _ _ ->
                            fillMInputFromRuleTree selectedRuleTree model

                        _ ->
                            termAndRuleDoNotMatchUp

                SigmaInput ->
                    let
                        latestTypingForX =
                            case thisTerm of
                                Abs var _ ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestXType -> Just { model | sigmaInput = showType latestXType })

                                _ ->
                                    Nothing

                        leftTypeFromThisNode =
                            case getLeftTypeFromRuleTree selectedRuleTree of
                                Just Untyped ->
                                    Nothing

                                Just leftType ->
                                    Just { model | sigmaInput = showType leftType }

                                _ ->
                                    Nothing

                        unusedTypeVar =
                            case getUnusedTypeVar 0 of
                                Just newTypeVar ->
                                    { model | sigmaInput = newTypeVar }

                                Nothing ->
                                    tooManyTypeVarInUse
                    in
                    case ( thisTerm, latestTypingForX, leftTypeFromThisNode ) of
                        ( Abs _ _, Just newModel, _ ) ->
                            newModel

                        ( Abs _ _, _, Just newModel ) ->
                            newModel

                        ( Abs _ _, _, _ ) ->
                            unusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                TauInput ->
                    let
                        latestTypingForMIfVar =
                            case thisTerm of
                                Abs _ (Var var) ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestMType -> Just { model | tauInput = showType latestMType })

                                _ ->
                                    Nothing

                        rightTypeFromThisNode =
                            case getRightTypeFromRuleTree selectedRuleTree of
                                Just Untyped ->
                                    Nothing

                                Just rightType ->
                                    Just { model | tauInput = showType rightType }

                                _ ->
                                    Nothing

                        unusedTypeVar =
                            case getUnusedTypeVar 1 of
                                Just newTypeVar ->
                                    { model | tauInput = newTypeVar }

                                Nothing ->
                                    tooManyTypeVarInUse
                    in
                    case ( thisTerm, latestTypingForMIfVar, rightTypeFromThisNode ) of
                        ( Abs _ _, Just newModel, _ ) ->
                            newModel

                        ( Abs _ _, _, Just newModel ) ->
                            newModel

                        ( Abs _ _, _, _ ) ->
                            unusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                _ ->
                    model

        ( RApp _ thisTerm thisType nextRuleTree1 nextRuleTree2, AppRule ) ->
            case inputKind of
                GammaInput ->
                    gammaHint

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
                        latestLeftTypeFromM =
                            case thisTerm of
                                App (Abs var _) _ ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestMLeftType -> Just { model | sigmaInput = showType latestMLeftType })

                                App (Var var) _ ->
                                    case Dict.get var model.latestTypings of
                                        Just (Arrow left _) ->
                                            Just { model | sigmaInput = showType left }

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        latestTypeFromN =
                            case thisTerm of
                                App _ (Var var) ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestNType -> Just { model | sigmaInput = showType latestNType })

                                _ ->
                                    Nothing

                        sigmaTypeFromArbitraryUpperTerms =
                            -- try the left RuleTree first
                            case getLeftTypeFromRuleTree nextRuleTree1 of
                                Just Untyped ->
                                    Nothing

                                Just leftType ->
                                    Just { model | sigmaInput = showType leftType }

                                _ ->
                                    -- try the right RuleTree
                                    case getTermTypeFromRuleTree nextRuleTree2 of
                                        Just Untyped ->
                                            Nothing

                                        Just typ ->
                                            Just { model | sigmaInput = showType typ }

                                        _ ->
                                            Nothing

                        unusedTypeVar =
                            case getUnusedTypeVar 0 of
                                Just newTypeVar ->
                                    { model | sigmaInput = newTypeVar }

                                Nothing ->
                                    tooManyTypeVarInUse
                    in
                    -- using nested tuples here just because Elm v0.19.1 does not allow tuples with more than 3 values
                    case ( thisTerm, latestLeftTypeFromM, ( latestTypeFromN, sigmaTypeFromArbitraryUpperTerms ) ) of
                        ( App _ _, Just newModel, ( _, _ ) ) ->
                            newModel

                        ( App _ _, _, ( Just newModel, _ ) ) ->
                            newModel

                        ( App _ _, _, ( _, Just newModel ) ) ->
                            newModel

                        ( App _ _, _, ( _, _ ) ) ->
                            unusedTypeVar

                        _ ->
                            termAndRuleDoNotMatchUp

                TauInput ->
                    let
                        latestTypeFromM =
                            case thisTerm of
                                App (Var var) _ ->
                                    case Dict.get var model.latestTypings of
                                        Just (Arrow _ right) ->
                                            Just { model | tauInput = showType right }

                                        _ ->
                                            Nothing

                                App (Abs _ (Var var)) _ ->
                                    Dict.get var model.latestTypings
                                        |> Maybe.andThen (\latestMType -> Just { model | tauInput = showType latestMType })

                                _ ->
                                    Nothing

                        tauTypeFromArbitraryMTerm =
                            case thisType of
                                Untyped ->
                                    Nothing

                                typ ->
                                    Just { model | tauInput = showType typ }

                        unusedTypeVar =
                            case getUnusedTypeVar 1 of
                                Just newTypeVar ->
                                    { model | tauInput = newTypeVar }

                                Nothing ->
                                    tooManyTypeVarInUse
                    in
                    case ( thisTerm, latestTypeFromM, tauTypeFromArbitraryMTerm ) of
                        ( App _ _, Just newModel, _ ) ->
                            newModel

                        ( App _ _, _, Just newModel ) ->
                            newModel

                        ( App _ _, _, _ ) ->
                            unusedTypeVar

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


{-| To be called immediately after updating a `ruleTree`
in order to update the `latestTypings` for all new **TermVar : SType** typings.
Tracks all new **TermVar:SType** typings from both the context and the nodes
(TermVar:SType)-typing itself (if available).
-}
updateLatestTypings : Dict TermVar SType -> RuleTree -> Bool -> Dict TermVar SType
updateLatestTypings latestTypingsParam ruleTree alsoCallOnChildren =
    let
        latestContextDict =
            case getContextFromRuleTree ruleTree of
                Context dict ->
                    dict

        -- updated typings from context
        newLatestTypings =
            Dict.foldl (\var typ latestTypings -> Dict.insert var typ latestTypings) latestTypingsParam latestContextDict

        updateLatestTypingsWith var typ latestTypings =
            Dict.insert var typ latestTypings
    in
    case ruleTree of
        RVar _ (Var var) typ _ ->
            updateLatestTypingsWith var typ newLatestTypings

        RAbs _ (Abs var _) (Arrow left _) nextRuleTree ->
            if alsoCallOnChildren then
                -- recursive call on the children to also update the model that we are going to return here
                updateLatestTypings (updateLatestTypingsWith var left newLatestTypings) nextRuleTree False

            else
                updateLatestTypingsWith var left newLatestTypings

        RApp _ _ _ nextRuleTree1 nextRuleTree2 ->
            if alsoCallOnChildren then
                -- recursive call on both children to both also update the model that we are going to return here
                updateLatestTypings
                    (updateLatestTypings newLatestTypings nextRuleTree2 False)
                    nextRuleTree1
                    False

            else
                newLatestTypings

        _ ->
            newLatestTypings


{-| Updates all **contexts** and all the nodes **(Term:Type)-typings** based on
the most recent changes tracked by `latestChanges`.

Also works on Abstractions, i.e.:

`(Abs var term) : (Arrow oldLeft oldRight)` becomes `(Abs var term) : (Arrow newLeft oldRight)`

if `var:newLeft` is in `latestChanges`

-}
applyLatestTypingsToFullRuleTree : Dict TermVar SType -> RuleTree -> RuleTree
applyLatestTypingsToFullRuleTree latestTypings ruleTree =
    let
        currentContextDict =
            case getContextFromRuleTree ruleTree of
                Context dict ->
                    dict

        newContext =
            Dict.foldl
                (\var typ newDict -> Dict.insert var (Dict.get var latestTypings |> Maybe.withDefault typ) newDict)
                Dict.empty
                currentContextDict
                |> Context
    in
    case ruleTree of
        RVar _ (Var var) typ hasBeenApplied ->
            RVar
                newContext
                (Var var)
                (Dict.get var latestTypings |> Maybe.withDefault typ)
                hasBeenApplied

        RVar _ a b c ->
            RVar newContext a b c

        RAbs _ ((Abs var _) as term) ((Arrow _ right) as typ) nextRuleTree ->
            RAbs
                newContext
                term
                (Dict.get var latestTypings |> Maybe.map (\updatedLeft -> Arrow updatedLeft right) |> Maybe.withDefault typ)
                (applyLatestTypingsToFullRuleTree latestTypings nextRuleTree)

        RAbs _ a b nextRuleTree ->
            RAbs newContext a b (applyLatestTypingsToFullRuleTree latestTypings nextRuleTree)

        RApp _ term typ nextRuleTree1 nextRuleTree2 ->
            RApp
                newContext
                term
                typ
                (applyLatestTypingsToFullRuleTree latestTypings nextRuleTree1)
                (applyLatestTypingsToFullRuleTree latestTypings nextRuleTree2)

        Hole ->
            Hole


{-| Traverses `ruleTree` and returns the type for `var` from the first context if available, otherwise from the parents context.
-}
getTypeForVarFromFirstContextMatch : TermVar -> RuleTree -> Maybe SType
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


{-| Returns a set of all type variable names being used in the given `ruleTree`.
-}
getUsedTypeVariables : RuleTree -> Set String
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


{-| Contains latin alphabet with none, one, two, and three primes, e.g. `a`, `a'`, `x''`, `e'''`.
The Cardinality is **96** = 4 \* 24.
-}
setOfUnusedTypeVariables : Set String
setOfUnusedTypeVariables =
    let
        baseNames =
            List.map String.fromChar lowerCaseLatinAlphabet
    in
    baseNames
        ++ List.map (\baseName -> baseName ++ "'") baseNames
        ++ List.map (\baseName -> baseName ++ "''") baseNames
        ++ List.map (\baseName -> baseName ++ "'''") baseNames
        |> Set.fromList


{-| Returns an unused type variable name from the set of
the latin alphabet with none, one, two, and three primes
(e.g. `a`, `a'`, `x''`, `e'''`).

Sorted from no primes to three primes, alphabetically, i.e.
if no type variable name are in use in given `ruleTree`, `index = 0` would return `a`
and `index = 95` would return `z'''`.

-}
getUnusedTypeVariableFromRuleTree : RuleTree -> Int -> Maybe String
getUnusedTypeVariableFromRuleTree ruleTree index =
    getUsedTypeVariables ruleTree
        |> Set.diff setOfUnusedTypeVariables
        |> Set.toList
        |> List.sortBy (\name -> String.toList name |> List.map Char.toCode |> List.sum)
        |> Array.fromList
        |> Array.get index
