module Hint exposing (..)

import Array
import Dict exposing (Dict)
import Json.Decode exposing (dict)
import RuleTree exposing (..)
import Set exposing (Set)
import SharedStructures exposing (..)
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

        contextDictUpdatedToLatestTypings =
            Dict.foldl
                (\var typ newDict -> Dict.insert var (Dict.get var model.latestTermVarTypings |> Maybe.withDefault typ) newDict)
                Dict.empty
                currentContextDict
    in
    case ( selectedRuleTree, model.menuState ) of
        ( RVar _ thisTerm thisType _, VarRule ) ->
            case inputKind of
                GammaInput ->
                    case thisTerm of
                        -- adds the typing assumption <var:thisType> in case it is missing
                        Var var ->
                            Dict.insert var thisType contextDictUpdatedToLatestTypings
                                |> Context
                                |> showContext
                                |> (\hintedContext -> { model | gammaInput = hintedContext })

                        _ ->
                            contextDictUpdatedToLatestTypings
                                |> Context
                                |> showContext
                                |> (\hintedContext -> { model | gammaInput = hintedContext })

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
                                    Dict.get var model.latestTermVarTypings
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

        ( RAbs _ thisTerm _ childRuleTree, AbsRule ) ->
            case inputKind of
                GammaInput ->
                    addNewTypingAssumps (getContextFromRuleTree childRuleTree) contextDictUpdatedToLatestTypings
                        |> Context
                        |> showContext
                        |> (\hintedContext -> { model | gammaInput = hintedContext })

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
                                    Dict.get var model.latestTermVarTypings
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
                                    Dict.get var model.latestTermVarTypings
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

        ( RApp _ thisTerm thisType childRuleTree1 childRuleTree2, AppRule ) ->
            case inputKind of
                GammaInput ->
                    addNewTypingAssumps (getContextFromRuleTree childRuleTree1) contextDictUpdatedToLatestTypings
                        |> addNewTypingAssumps (getContextFromRuleTree childRuleTree2)
                        |> Context
                        |> showContext
                        |> (\hintedContext -> { model | gammaInput = hintedContext })

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
                                    Dict.get var model.latestTermVarTypings
                                        |> Maybe.andThen (\latestMLeftType -> Just { model | sigmaInput = showType latestMLeftType })

                                App (Var var) _ ->
                                    case Dict.get var model.latestTermVarTypings of
                                        Just (Arrow left _) ->
                                            Just { model | sigmaInput = showType left }

                                        _ ->
                                            Nothing

                                _ ->
                                    Nothing

                        latestTypeFromN =
                            case thisTerm of
                                App _ (Var var) ->
                                    Dict.get var model.latestTermVarTypings
                                        |> Maybe.andThen (\latestNType -> Just { model | sigmaInput = showType latestNType })

                                _ ->
                                    Nothing

                        sigmaTypeFromArbitraryUpperTerms =
                            -- try the left RuleTree first
                            case getLeftTypeFromRuleTree childRuleTree1 of
                                Just Untyped ->
                                    Nothing

                                Just leftType ->
                                    Just { model | sigmaInput = showType leftType }

                                _ ->
                                    -- try the right RuleTree
                                    case getTermTypeFromRuleTree childRuleTree2 of
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
                                    case Dict.get var model.latestTermVarTypings of
                                        Just (Arrow _ right) ->
                                            Just { model | tauInput = showType right }

                                        _ ->
                                            Nothing

                                App (Abs _ (Var var)) _ ->
                                    Dict.get var model.latestTermVarTypings
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


{-| Typings Assumptions that are in `fromDict` but not in `toDict` get added to `toDict`.
-}
addNewTypingAssumps : SContext -> Dict TermVar SType -> Dict TermVar SType
addNewTypingAssumps (Context fromDict) toDict =
    Dict.foldl
        (\var typ resultDict ->
            if not (Dict.member var resultDict) then
                Dict.insert var typ resultDict

            else
                resultDict
        )
        toDict
        fromDict


{-| To be called immediately after updating a `ruleTree`
in order to update the `latestTermVarTypings` for all new **TermVar : SType** typings.
Tracks all new **TermVar:SType** typings from both the context and the nodes
(TermVar:SType)-typing itself (if available).
Typings for terms other than _TermVar_ are not being tracked.
-}
updateLatestTermVarTypings : Dict TermVar SType -> RuleTree -> Bool -> Dict TermVar SType
updateLatestTermVarTypings latestTermVarTypingsParam ruleTree alsoCallOnChildren =
    let
        latestContextDict =
            case getContextFromRuleTree ruleTree of
                Context dict ->
                    dict

        updateLatestTypingsWith var typ latestTypings =
            -- do not update with the most unknown types
            if typ /= Untyped && typ /= Arrow Untyped Untyped then
                Dict.insert var typ latestTypings

            else
                latestTypings

        -- updated typings from context
        newLatestTypings =
            Dict.foldl (\var typ latestTypings -> updateLatestTypingsWith var typ latestTypings) latestTermVarTypingsParam latestContextDict
    in
    case ruleTree of
        RVar _ (Var var) typ _ ->
            updateLatestTypingsWith var typ newLatestTypings

        RAbs _ (Abs var _) (Arrow left _) childRuleTree ->
            if alsoCallOnChildren then
                -- recursive call on the children to also update the model that we are going to return here
                updateLatestTermVarTypings (updateLatestTypingsWith var left newLatestTypings) childRuleTree False

            else
                updateLatestTypingsWith var left newLatestTypings

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            if alsoCallOnChildren then
                -- recursive call on both children to both also update the model that we are going to return here
                updateLatestTermVarTypings
                    (updateLatestTermVarTypings newLatestTypings childRuleTree2 False)
                    childRuleTree1
                    False

            else
                newLatestTypings

        _ ->
            newLatestTypings


{-| Updates all **contexts** and all the nodes **(Term:Type)-typings** based on
the most recent changes tracked by `latestTermVarTypings`.

Also works on Abstractions, i.e.:

`(Abs var term) : (Arrow oldLeft oldRight)` becomes `(Abs var term) : (Arrow newLeft oldRight)`

if `var:newLeft` is in `latestTermVarTypings`

-}
applyLatestTermVarTypingsToFullRuleTree : Dict TermVar SType -> RuleTree -> RuleTree
applyLatestTermVarTypingsToFullRuleTree latestTermVarTypings ruleTree =
    let
        currentContextDict =
            case getContextFromRuleTree ruleTree of
                Context dict ->
                    dict

        newContext =
            Dict.foldl
                (\var typ newDict -> Dict.insert var (Dict.get var latestTermVarTypings |> Maybe.withDefault typ) newDict)
                Dict.empty
                currentContextDict
                |> Context
    in
    case ruleTree of
        RVar _ (Var var) typ hasBeenApplied ->
            RVar
                newContext
                (Var var)
                (Dict.get var latestTermVarTypings |> Maybe.withDefault typ)
                hasBeenApplied

        RVar _ a b c ->
            RVar newContext a b c

        RAbs _ ((Abs var _) as term) ((Arrow _ right) as typ) childRuleTree ->
            RAbs
                newContext
                term
                (Dict.get var latestTermVarTypings |> Maybe.map (\updatedLeft -> Arrow updatedLeft right) |> Maybe.withDefault typ)
                (applyLatestTermVarTypingsToFullRuleTree latestTermVarTypings childRuleTree)

        RAbs _ a b childRuleTree ->
            RAbs newContext a b (applyLatestTermVarTypingsToFullRuleTree latestTermVarTypings childRuleTree)

        RApp _ term typ childRuleTree1 childRuleTree2 ->
            RApp
                newContext
                term
                typ
                (applyLatestTermVarTypingsToFullRuleTree latestTermVarTypings childRuleTree1)
                (applyLatestTermVarTypingsToFullRuleTree latestTermVarTypings childRuleTree2)

        Hole ->
            Hole


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


{-| Propagates typing changes through the whole RuleTree given by `root`.
See `passChangesDownwards` and `passChangesUpwards`.
-}
passChangesThroughRuleTree : RuleTree -> List Int -> RuleTree -> RuleTree
passChangesThroughRuleTree ruleTree nodeId root =
    passChangesDownwards (passChangesUpwards ruleTree) nodeId root


{-| Propagates typing changes downwards according to the type inference rules.
-}
passChangesUpwards : RuleTree -> RuleTree
passChangesUpwards ruleTree =
    if isLeaf ruleTree then
        ruleTree

    else
        case ruleTree of
            RAbs context ((Abs var _) as term) ((Arrow sigma tau) as typ) oldChildRuleTree ->
                let
                    newChildContext =
                        updateContext (addTypingAssumptionToContext var sigma context) (getContextFromRuleTree oldChildRuleTree)

                    newChildRuleTree =
                        reconstructOnPassedChanges oldChildRuleTree newChildContext (Just ( tau, FullType )) Nothing Nothing
                            |> passChangesUpwards
                in
                RAbs context term typ newChildRuleTree

            RApp context term typ oldChildRuleTree1 oldChildRuleTree2 ->
                let
                    newChildContext1 =
                        updateContext context (getContextFromRuleTree oldChildRuleTree1)

                    newChildRuleTree1 =
                        reconstructOnPassedChanges oldChildRuleTree1 newChildContext1 (Just ( typ, ArrRight )) Nothing Nothing
                            |> passChangesUpwards

                    newChildContext2 =
                        updateContext context (getContextFromRuleTree oldChildRuleTree2)

                    newChildRuleTree2 =
                        reconstructOnPassedChanges oldChildRuleTree2 newChildContext2 Nothing Nothing Nothing
                            |> passChangesUpwards
                in
                RApp context term typ newChildRuleTree1 newChildRuleTree2

            _ ->
                ruleTree


{-| Propagates typing changes downwards according to the type inference rules.
For neighbour nodes on the application rule it also initiates the propagation of changes upwards, e.g.:

`child1 child2`

`--------------`

`parent`

If `passChangesDownwards` is called on `child2` then it proceeds to call `passChangesUpwards` on `child1`
and `passChangesDownwards` on `parent`.

-}
passChangesDownwards : RuleTree -> List Int -> RuleTree -> RuleTree
passChangesDownwards ruleTree nodeId root =
    let
        _ =
            Debug.log "currentNodeId" nodeId

        parentNodeId =
            List.take (List.length nodeId - 1) nodeId |> Debug.log "parentNodeId"

        -- to decide wether the ruleTree is the left child (0) or right child (1) of an RApp parent
        lastNodePointer =
            List.reverse nodeId |> List.head |> Maybe.withDefault -1

        parentRuleTree =
            getRuleTreeNode root parentNodeId

        context =
            getContextFromRuleTree ruleTree

        leftType =
            getLeftTypeFromRuleTree ruleTree

        rightType =
            getRightTypeFromRuleTree ruleTree

        fullType =
            getTermTypeFromRuleTree ruleTree

        typeChangeInfo typ typePointer =
            case typ of
                Just typ_ ->
                    Just ( typ_, typePointer )

                _ ->
                    Nothing
    in
    case ( parentRuleTree, lastNodePointer, nodeId ) of
        -- reached the root node
        ( _, _, [] ) ->
            ruleTree

        ( RAbs parentContext (Abs var _) _ _, _, _ ) ->
            let
                newLeftType =
                    getTypeFromContext var context

                -- takes the variables type as left type (sigma in inference rule), and terms type as right type (tau in inference rule)
                newArrowType =
                    Maybe.map2 (\left right -> Arrow left right) newLeftType fullType
            in
            reconstructOnPassedChanges parentRuleTree
                (updateContext context parentContext)
                (typeChangeInfo newArrowType FullType)
                (Just ruleTree)
                Nothing
                |> (\newlyBuiltParentRuleTree -> passChangesDownwards newlyBuiltParentRuleTree parentNodeId root)

        ( RApp parentContext _ _ _ parentsNext2, 0, _ ) ->
            let
                newContextForParentsNext2 =
                    updateContext context <| getContextFromRuleTree parentsNext2
            in
            reconstructOnPassedChanges parentRuleTree
                (updateContext context parentContext)
                (typeChangeInfo rightType FullType)
                (Just ruleTree)
                (reconstructOnPassedChanges parentsNext2 newContextForParentsNext2 (typeChangeInfo leftType FullType) Nothing Nothing |> passChangesUpwards |> Just)
                |> (\newlyBuiltParentRuleTree -> passChangesDownwards newlyBuiltParentRuleTree parentNodeId root)

        ( RApp parentContext _ _ parentsNext1 _, 1, _ ) ->
            let
                newContextForParentsNext1 =
                    updateContext context <| getContextFromRuleTree parentsNext1
            in
            reconstructOnPassedChanges parentRuleTree
                (updateContext context parentContext)
                Nothing
                (reconstructOnPassedChanges parentsNext1 newContextForParentsNext1 (typeChangeInfo fullType ArrLeft) Nothing Nothing |> passChangesUpwards |> Just)
                (ruleTree |> Just)
                |> (\newlyBuiltParentRuleTree -> passChangesDownwards newlyBuiltParentRuleTree parentNodeId root)

        -- dont change anything, except that the parent now has the newly changed child 'ruleTree'
        ( RAbs a b c _, _, _ ) ->
            RAbs a b c ruleTree |> (\newlyBuiltParentRuleTree -> passChangesDownwards newlyBuiltParentRuleTree parentNodeId root)

        _ ->
            Hole


{-| Reconstructs `ruleTree` with `newContext`, a new term typing defined by `typeChangeInfo`
and new child nodes `newChild1` and `newChild1`.

`typeChangeInfo` only gets applied if it can fit into the type of `ruleTree`,
i.e. if `TypePointer` is `ArrLeft` or `ArrRight`, then type of `ruleTree` must have form `Arrow`,
otherwise no type changes are made.

**Example for typeChangeInfo**

`typeChangeInfo == { Bool, ArrLeft }; termType == Int -> String => newTermType == Bool -> String`

-}
reconstructOnPassedChanges : RuleTree -> SContext -> Maybe ( SType, TypePointer ) -> Maybe RuleTree -> Maybe RuleTree -> RuleTree
reconstructOnPassedChanges ruleTree newContext typeChangeInfo newChild1 newChild2 =
    let
        oldType =
            -- the ruleTree given to this function should never be 'Hole' so getTermTypeFromRuleTree should never return 'Nothing'
            getTermTypeFromRuleTree ruleTree |> Maybe.withDefault Untyped

        newType =
            case ( typeChangeInfo, oldType ) of
                ( Nothing, _ ) ->
                    oldType

                ( Just ( newFull, FullType ), _ ) ->
                    newFull

                ( Just ( newLeft, ArrLeft ), Arrow _ oldRight ) ->
                    Arrow newLeft oldRight

                ( Just ( newRight, ArrRight ), Arrow oldLeft _ ) ->
                    Arrow oldLeft newRight

                _ ->
                    oldType
    in
    case ruleTree of
        RVar _ term _ hasBeenApplied ->
            RVar newContext term newType hasBeenApplied

        RAbs _ term _ childRuleTree ->
            RAbs newContext term newType (Maybe.withDefault childRuleTree newChild1)

        RApp _ term _ childRuleTree1 childRuleTree2 ->
            RApp newContext term newType (Maybe.withDefault childRuleTree1 newChild1) (Maybe.withDefault childRuleTree2 newChild2)

        _ ->
            ruleTree


{-| Updates `(Context dictTo)` from `(Context dictFrom)` only on typings assumptions that are
in both contexts. Other typing assumptions in `(Context dictTo)` remain unchanged.

**Example**

    updateContext (x:a, y:b) (x:b, y:b, z:c) => (x:a, y:b, z:c)

-}
updateContext : SContext -> SContext -> SContext
updateContext (Context dictFrom) (Context dictTo) =
    Dict.foldl
        (\var typ resultDict ->
            if Dict.member var resultDict then
                Dict.insert var typ resultDict

            else
                resultDict
        )
        dictTo
        dictFrom
        |> Context


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
