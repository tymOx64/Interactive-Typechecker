module RuleTree exposing (..)

--exposing (RuleTree, SContext, Term, SType, Type, Context, Var, r00, showRules, showTerm, showRuleContent, t1)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (dict)
import RuleTreeUtils exposing (getContextFromRuleTree, getLeftTypeFromRuleTree, getRightTypeFromRuleTree, getRuleTreeNode, getTermFromRuleTree, getTermTypeFromRuleTree)
import Set exposing (Set)
import SharedStructures exposing (..)
import Utils exposing (contextsAreEqual, getSuccessEmoji, getTypeFromContext, varIsMissingInContext, variableAndTypeConflictExistingTypingAssumption)


{-| Traverses given `ruleTree` and all of its child nodes in order to view them in tree structure.

Nodes get highlighted according to given `pointersToHighlight`.

`nodeId` should correspond to the given `ruleTree`, i.e. `[]` for the root node.

-}
viewRuleTree : RuleTree -> List Int -> Model -> List Pointer -> Html Msg
viewRuleTree ruleTree nodeId model pointersToHighlight =
    let
        onClickSelect =
            onClick <| SelectTreeNode <| nodeId

        nodeAttributes =
            [ classList [ ( "ruletree__conclusion", True ), ( "ruletree__conclusion--selected", nodeId == model.selectedNodeId ) ]
            , onClickSelect
            , onDoubleClick <| ResetTreeNode <| nodeId
            , title "Select Tree Node"
            ]

        pointersForCurrentRuleTree =
            List.foldl
                (\conflictPointer list ->
                    if getNodeIdFromPointer conflictPointer == nodeId then
                        conflictPointer :: list

                    else
                        list
                )
                []
                pointersToHighlight
    in
    case ruleTree of
        RVar context term typ hasBeenApplied ->
            let
                premise =
                    if model.ruleTreeSuccessful then
                        div [ class "ruletree__hole", onClickSelect ] [ text (getSuccessEmoji <| String.length (showContext context)) ]

                    else if hasBeenApplied then
                        text ""

                    else
                        div [ class "ruletree__hole", onClickSelect ] [ text "?" ]
            in
            div [ class "ruletree__rule-container" ]
                [ premise
                , div [ class "ruletree__line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree model.viewLatinChar)
                ]

        RAbs context term typ childRuleTree ->
            let
                premise =
                    if childRuleTree == Hole then
                        div [ class "ruletree__hole", onClickSelect ] [ text "?" ]

                    else
                        div [] [ viewRuleTree childRuleTree (nodeId ++ [ 0 ]) model pointersToHighlight ]
            in
            div [ class "ruletree__rule-container" ]
                [ premise
                , div [ class "ruletree__line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree model.viewLatinChar)
                ]

        RApp context term typ childRuleTree1 childRuleTree2 ->
            let
                premise =
                    if childRuleTree1 == Hole && childRuleTree2 == Hole then
                        div [ class "ruletree__hole", onClickSelect ] [ text "?" ]

                    else
                        div []
                            [ div [ class "ruletree__split-up-node" ]
                                [ viewRuleTree childRuleTree1 (nodeId ++ [ 0 ]) model pointersToHighlight ]
                            , div [ class "ruletree__split-up-node" ]
                                [ viewRuleTree childRuleTree2 (nodeId ++ [ 1 ]) model pointersToHighlight ]
                            ]
            in
            div [ class "ruletree__rule-container" ]
                [ premise
                , div [ class "ruletree__line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree model.viewLatinChar)
                ]

        Hole ->
            div [ class "ruletree__rule-container" ]
                [ text "?" ]


{-| Collects all conflicts into a list.
A conflict itself is a `List Pointer`, e.g. the following is a conflict saying that
the left term of the Application at `nodeId` is in conflict with its leftmost childs full term:

`[ [ TermPointer nodeId AppLeft, TermPointer (nodeId ++ [ 0 ]) FullTerm ] ]`

-}
getConflictsInRuleTree : RuleTree -> List Int -> List (List Pointer)
getConflictsInRuleTree ruleTree nodeId =
    let
        appendIfConditionHolds condition conflictList =
            if condition then
                conflictList

            else
                []
    in
    case ruleTree of
        RVar context (Var var) typ _ ->
            appendIfConditionHolds
                (variableAndTypeConflictExistingTypingAssumption var typ context)
                [ [ ContPointer nodeId (FullAssump var), TermAndType nodeId ] ]
                ++ appendIfConditionHolds
                    (varIsMissingInContext var context)
                    [ [ ContPointer nodeId FullContext, TermAndType nodeId ] ]

        RVar _ _ _ _ ->
            [ [ TermPointer nodeId FullTerm ] ]

        RAbs _ (Abs _ _) (Arrow _ _) Hole ->
            []

        RAbs _ _ (Arrow _ _) Hole ->
            [ [ TermPointer nodeId FullTerm ] ]

        RAbs _ (Abs _ _) _ Hole ->
            [ [ TypePointer nodeId FullType ] ]

        RAbs _ _ _ Hole ->
            [ [ TermPointer nodeId FullTerm ], [ TypePointer nodeId FullType ] ]

        RAbs context (Abs var mTerm) (Arrow sigma tau) childRuleTree ->
            let
                removeVarFromContext (Context dict) =
                    Context <| Dict.remove var dict

                childContextWithOutAbstractionVar =
                    removeVarFromContext (getContextFromRuleTree childRuleTree)

                sigmaIsConflicting =
                    Maybe.map2 (/=) (Just sigma) (getTypeFromContext var (getContextFromRuleTree childRuleTree))
                        |> Maybe.withDefault True

                tauIsConflicting =
                    Maybe.map2 (/=) (Just tau) (getTermTypeFromRuleTree childRuleTree)
                        |> Maybe.withDefault True

                -- checks for equality for both cases (1) x:sigma is not in the parents context (2) x:sigma is already in the parents context
                -- if either of both cases is true, no conflict will be tracked
                contextIsConflicting =
                    not <|
                        (contextsAreEqual context childContextWithOutAbstractionVar
                            || contextsAreEqual context (getContextFromRuleTree childRuleTree)
                        )

                mTermIsConflicting =
                    getTermFromRuleTree childRuleTree |> Maybe.andThen (\term -> Just <| term /= mTerm) |> Maybe.withDefault False
            in
            appendIfConditionHolds
                sigmaIsConflicting
                [ [ TypePointer nodeId ArrLeft, ContPointer (nodeId ++ [ 0 ]) FullContext ] ]
                ++ appendIfConditionHolds
                    tauIsConflicting
                    [ [ TypePointer nodeId ArrRight, TypePointer (nodeId ++ [ 0 ]) FullType ] ]
                ++ appendIfConditionHolds
                    contextIsConflicting
                    [ [ ContPointer nodeId FullContext, ContPointer (nodeId ++ [ 0 ]) FullContext ] ]
                ++ appendIfConditionHolds
                    mTermIsConflicting
                    [ [ TermPointer nodeId AbsBody, TermPointer (nodeId ++ [ 0 ]) FullTerm ] ]
                ++ getConflictsInRuleTree childRuleTree (nodeId ++ [ 0 ])

        RAbs _ _ (Arrow _ _) childRuleTree ->
            [ TermPointer nodeId FullTerm ] :: getConflictsInRuleTree childRuleTree (nodeId ++ [ 0 ])

        RAbs _ (Abs _ _) _ childRuleTree ->
            [ TypePointer nodeId FullType ] :: getConflictsInRuleTree childRuleTree (nodeId ++ [ 0 ])

        RAbs _ _ _ childRuleTree ->
            [ [ TermPointer nodeId FullTerm ], [ TypePointer nodeId FullType ] ] ++ getConflictsInRuleTree childRuleTree (nodeId ++ [ 0 ])

        RApp _ (App _ _) _ Hole Hole ->
            []

        RApp _ _ _ Hole Hole ->
            [ [ TermPointer nodeId FullTerm ] ]

        RApp context (App mTerm nTerm) tau childRuleTree1 childRuleTree2 ->
            let
                tauIsConflicting =
                    Just tau /= getRightTypeFromRuleTree childRuleTree1

                mTermIsConflicting =
                    getTermFromRuleTree childRuleTree1 |> Maybe.andThen (\childMTerm -> Just <| childMTerm /= mTerm) |> Maybe.withDefault False

                nTermIsConflicting =
                    getTermFromRuleTree childRuleTree2 |> Maybe.andThen (\childNTerm -> Just <| childNTerm /= nTerm) |> Maybe.withDefault False

                leftContextIsConflicting =
                    not <| contextsAreEqual context <| getContextFromRuleTree childRuleTree1

                rightContextIsConflicting =
                    not <| contextsAreEqual context <| getContextFromRuleTree childRuleTree2

                upperSigmaIsConflicting =
                    Maybe.map2 (/=) (getLeftTypeFromRuleTree childRuleTree1) (getTermTypeFromRuleTree childRuleTree2)
                        |> Maybe.withDefault True
            in
            appendIfConditionHolds
                tauIsConflicting
                [ [ TypePointer nodeId FullType, TypePointer (nodeId ++ [ 0 ]) ArrRight ] ]
                ++ appendIfConditionHolds
                    mTermIsConflicting
                    [ [ TermPointer nodeId AppLeft, TermPointer (nodeId ++ [ 0 ]) FullTerm ] ]
                ++ appendIfConditionHolds
                    nTermIsConflicting
                    [ [ TermPointer nodeId AppRight, TermPointer (nodeId ++ [ 1 ]) FullTerm ] ]
                ++ appendIfConditionHolds
                    leftContextIsConflicting
                    [ [ ContPointer nodeId FullContext, ContPointer (nodeId ++ [ 0 ]) FullContext ] ]
                ++ appendIfConditionHolds
                    rightContextIsConflicting
                    [ [ ContPointer nodeId FullContext, ContPointer (nodeId ++ [ 1 ]) FullContext ] ]
                ++ appendIfConditionHolds
                    upperSigmaIsConflicting
                    [ [ TypePointer (nodeId ++ [ 0 ]) ArrLeft, TypePointer (nodeId ++ [ 1 ]) FullType ] ]
                ++ getConflictsInRuleTree childRuleTree1 (nodeId ++ [ 0 ])
                ++ getConflictsInRuleTree childRuleTree2 (nodeId ++ [ 1 ])

        RApp _ _ _ _ _ ->
            [ [ TermPointer nodeId FullTerm ] ]

        Hole ->
            []


{-| Returns the first conflict within given `ruleTree`.
If there is none, an empty conflict will be returned, i.e. an empty list
of conflict pointers.

See function `getConflictsInRuleTree`.

-}
getFirstConflictFromRuleTree : RuleTree -> List Pointer
getFirstConflictFromRuleTree ruleTree =
    let
        isSingleton =
            case ruleTree of
                RVar _ _ _ _ ->
                    True

                RAbs _ _ _ Hole ->
                    True

                RApp _ _ _ Hole Hole ->
                    True

                _ ->
                    False
    in
    if isSingleton then
        []

    else
        getConflictsInRuleTree ruleTree [] |> List.head |> Maybe.withDefault []


{-| Replaces the `RuleTree` at given `nodeId` for given `singletonNewRuleTree`.

The initial call should be made with the root node as the argument for `ruleTree`.

If `keepOldChildren` is set to `True`, it will be tried to keep the 2nd generation children.
See function `keepChildrenOfOldChildrenIfTreeStructureIsUnchanged` for more details.

-}
changeRuleTreeNode : RuleTree -> List Int -> RuleTree -> Bool -> RuleTree
changeRuleTreeNode ruleTree nodeId singletonNewRuleTree keepOldChildren =
    let
        ruleTreeNodeToBeChanged =
            getRuleTreeNode ruleTree nodeId

        newRuleTree =
            if keepOldChildren then
                keepChildrenOfOldChildrenIfTreeStructureIsUnchanged ruleTreeNodeToBeChanged singletonNewRuleTree

            else
                singletonNewRuleTree
    in
    case ( ruleTree, nodeId ) of
        ( RAbs a b c childRuleTree, 0 :: childNodeId ) ->
            RAbs a b c <| changeRuleTreeNode childRuleTree childNodeId newRuleTree keepOldChildren

        ( RApp a b c childRuleTree d, 0 :: childNodeId ) ->
            (RApp a b c <| changeRuleTreeNode childRuleTree childNodeId newRuleTree keepOldChildren) d

        ( RApp a b c d childRuleTree, 1 :: childNodeId ) ->
            RApp a b c d <| changeRuleTreeNode childRuleTree childNodeId newRuleTree keepOldChildren

        ( _, [] ) ->
            newRuleTree

        -- this case can only get matched if given nodeId leads to a non existing ruletree node
        ( _, _ ) ->
            Hole


{-| Resets the ruletree at given `nodeId`, i.e. all of its children become `Hole`;
in case of `RVar` the value for `hasBeenApplied` gets set to `False`.

The argument for `ruleTree` should be the root node.

-}
resetRuleTreeNode : RuleTree -> List Int -> RuleTree
resetRuleTreeNode ruleTree nodeId =
    let
        ruleTreeNodeToBeResetted =
            getRuleTreeNode ruleTree nodeId
    in
    case ruleTreeNodeToBeResetted of
        RVar a b c _ ->
            changeRuleTreeNode ruleTree nodeId (RVar a b c False) False

        RAbs a b c _ ->
            changeRuleTreeNode ruleTree nodeId (RAbs a b c Hole) False

        RApp a b c _ _ ->
            changeRuleTreeNode ruleTree nodeId (RApp a b c Hole Hole) False

        Hole ->
            ruleTree


{-| Returns the target node id if there is one available, otherwise returns the currently selected node id (i.e. the selected node id remains unchanged).
The direction (left/right) is defined through given `isLeftKeyEvent`.
-}
getNodeIdForArrowLeftOrRightKeyEvent : RuleTree -> List Int -> Bool -> List Int
getNodeIdForArrowLeftOrRightKeyEvent ruleTree selectedNodeId isLeftKeyEvent =
    let
        targetLevel =
            List.length selectedNodeId

        filterPartsToTargetLevel parts =
            Tuple.pair
                (List.filter (\pair -> Tuple.second pair == targetLevel) (Tuple.first parts))
                (List.filter (\pair -> Tuple.second pair == targetLevel) (Tuple.second parts))

        leftPartAndRightPart =
            ruleTreeAsInOrderList ruleTree [] 0 [ -1 ] |> splitListAtNodeId selectedNodeId |> filterPartsToTargetLevel

        -- in case of right key event, swap the parts around to reverse the direction
        splittedInOrderParts =
            case ( leftPartAndRightPart, isLeftKeyEvent ) of
                ( _, True ) ->
                    leftPartAndRightPart

                ( ( left, right ), False ) ->
                    Tuple.pair right left
    in
    case splittedInOrderParts of
        ( [], [] ) ->
            selectedNodeId

        ( [], sidePart2 ) ->
            sidePart2 |> List.reverse |> List.head |> Maybe.withDefault ( selectedNodeId, -1 ) |> Tuple.first

        ( sidePart1, _ ) ->
            sidePart1 |> List.head |> Maybe.withDefault ( selectedNodeId, -1 ) |> Tuple.first


{-| For non-root nodes the function simply returns the parent node id for given `selectedNodeId`.

For the root node, the outmost node id at the top will be returned, i.e. the node id with the biggest length.

-}
getNodeIdForArrowDownKeyEvent : RuleTree -> List Int -> List Int
getNodeIdForArrowDownKeyEvent ruleTree selectedNodeId =
    let
        allNodeIds =
            ruleTreeAsInOrderList ruleTree [] 0 [ -1 ]

        outmostNodeId =
            List.foldl
                (\pair maxLengthNodeId ->
                    if List.length (Tuple.first pair) > List.length maxLengthNodeId then
                        Tuple.first pair

                    else
                        maxLengthNodeId
                )
                []
                allNodeIds
    in
    case selectedNodeId of
        [] ->
            outmostNodeId

        _ ->
            List.take (List.length selectedNodeId - 1) selectedNodeId


{-| Returns the leftmost child of given `selectedNodeId` if there is one.
Otherwise it finds the closest node id which is one level high of `selectedNodeId` if there is one.
If non of the above results in a node id, there is nothing above `selectedNodeId` and the
root node id of `[]` gets returned in order to jump back to the bottom.
-}
getNodeIdForArrowUpKeyEvent : RuleTree -> List Int -> List Int
getNodeIdForArrowUpKeyEvent ruleTree selectedNodeId =
    let
        targetLevel =
            List.length selectedNodeId + 1

        leftPartAndRightPart =
            ruleTreeAsInOrderList ruleTree [] 0 selectedNodeId |> splitListAtNodeId [ -1 ]

        firstMatch leftRightPair scanLeftNext =
            case leftRightPair of
                ( [], [] ) ->
                    []

                ( l :: ls, [] ) ->
                    if Tuple.second l == targetLevel then
                        Tuple.first l

                    else
                        firstMatch ( ls, [] ) True

                ( [], r :: rs ) ->
                    if Tuple.second r == targetLevel then
                        Tuple.first r

                    else
                        firstMatch ( [], rs ) False

                ( l :: ls, r :: rs ) ->
                    if scanLeftNext then
                        if Tuple.second l == targetLevel then
                            Tuple.first l

                        else
                            firstMatch ( ls, r :: rs ) False

                    else if Tuple.second r == targetLevel then
                        Tuple.first r

                    else
                        firstMatch ( l :: ls, rs ) True
    in
    if getRuleTreeNode ruleTree (selectedNodeId ++ [ 0 ]) /= Hole then
        selectedNodeId ++ [ 0 ]

    else
        firstMatch leftPartAndRightPart True


{-| Splits given `list` at `nodeId` into a left and right part.
Given `list` is supposed to be an inorder traversal where each node
gets stored as a `Tuple` of the form `(nodeId, level)` where `level`
is the level in the tree, e.g. root is at level 0, the roots children
at level 1, etc.
-}
splitListAtNodeId : List Int -> List ( List Int, Int ) -> ( List ( List Int, Int ), List ( List Int, Int ) )
splitListAtNodeId nodeId list =
    let
        splittingResult =
            List.foldl
                (\nodeIdPair splitData ->
                    if Tuple.first nodeIdPair == nodeId then
                        { splitData | onLeftSide = False }

                    else if splitData.onLeftSide then
                        { splitData | leftPart = nodeIdPair :: splitData.leftPart }

                    else
                        { splitData | rightPart = splitData.rightPart ++ [ nodeIdPair ] }
                )
                { leftPart = [], rightPart = [], onLeftSide = True }
                list
    in
    Tuple.pair splittingResult.leftPart splittingResult.rightPart


{-| Traverses `ruleTree` Inorder (i.e. _Left -> Root -> Right_ for two children; _Top -> Root_ for one children)
to add each node to the resulting `List` in form of a `Tuple` consisting of the nodes _nodeId_ and _level_.

The level starts at 0 for the lowest node.

In case of traversing an `RVar` node as the current user selected node an auxiliary `Tuple` gets added to the
`List` with an invalid _nodeId_ of [ -1 ] and (level + 1).
This simplifies the process of splitting up the resulting `List` into a left and right part for all
nodes on (level + 1).

-}
ruleTreeAsInOrderList : RuleTree -> List Int -> Int -> List Int -> List ( List Int, Int )
ruleTreeAsInOrderList ruleTree nodeId level currentUserSelectedNodeId =
    case ruleTree of
        RVar _ _ _ _ ->
            if nodeId /= currentUserSelectedNodeId then
                [ ( nodeId, level ) ]

            else
                [ ( [ -1 ], level + 1 ), ( nodeId, level ) ]

        RAbs _ _ _ childRuleTree ->
            ruleTreeAsInOrderList childRuleTree (nodeId ++ [ 0 ]) (level + 1) currentUserSelectedNodeId |> (++) [ ( nodeId, level ) ]

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            ruleTreeAsInOrderList childRuleTree1 (nodeId ++ [ 0 ]) (level + 1) currentUserSelectedNodeId
                |> (++) [ ( nodeId, level ) ]
                |> (++) (ruleTreeAsInOrderList childRuleTree2 (nodeId ++ [ 1 ]) (level + 1) currentUserSelectedNodeId)

        Hole ->
            []


{-| Keeps the children of the children (_2nd generation children_) if the tree structure **remains unchanged**.
The newly generated _1st generation children_ **remain new**. But the old 2nd generation children remain old if the
tree structure allows for that. If the tree structure doesn't allow for that, the 2nd generation gets removed and
all subsequent generations of the newRuleTree are being kept (which is usually just the one coming from the premise of the inference rule).

The idea behind this is that updating a tree nodes conclusion directly dictates what its premise looks like
(the premise is the 1st generation of children), so the premise gets updated as well.
However, the children of the premise (2nd generation) is yet to be decided on how they should change and
until then we want to keep the old 2nd generation (including all their subsequent generations).

-}
keepChildrenOfOldChildrenIfTreeStructureIsUnchanged : RuleTree -> RuleTree -> RuleTree
keepChildrenOfOldChildrenIfTreeStructureIsUnchanged oldRuleTree newRuleTree =
    case ( oldRuleTree, newRuleTree ) of
        ( RAbs _ _ _ oldChild, RAbs a b c newChild ) ->
            RAbs a b c <| keepOldChildrenIfRuleIsIdentical oldChild newChild

        ( RApp _ _ _ oldLeftChild oldRightChild, RApp a b c newLeftChild newRightChild ) ->
            RApp a b c (keepOldChildrenIfRuleIsIdentical oldLeftChild newLeftChild) (keepOldChildrenIfRuleIsIdentical oldRightChild newRightChild)

        ( _, _ ) ->
            newRuleTree


{-| If the rule is the same for the old and new RuleTree then append the children from the old to the new RuleTree, otherwise don't keep any children.
-}
keepOldChildrenIfRuleIsIdentical : RuleTree -> RuleTree -> RuleTree
keepOldChildrenIfRuleIsIdentical oldRuleTree newRuleTree =
    case ( oldRuleTree, newRuleTree ) of
        ( RAbs _ _ _ child, RAbs a b c _ ) ->
            RAbs a b c child

        ( RApp _ _ _ leftChild rightChild, RApp a b c _ _ ) ->
            RApp a b c leftChild rightChild

        ( _, _ ) ->
            newRuleTree


{-| Views given `Context` as defined through given `contextHandler`.
Highlightening can be defined through given `conflictPointers`.
Representation of type variables in latin or greek letters is defined
by `viewLatinChar`.
-}
viewContext :
    AContextHandler comparableVar typ
    -> AContext comparableVar typ
    -> List (APointer () (AContPointer comparableVar) termPointer typePointer)
    -> Bool
    -> List (Html Msg)
viewContext contextHandler (Context dict) conflictPointers viewLatinChar =
    let
        highlightFullNode =
            List.member (FullNode ()) conflictPointers

        highlightFullContext =
            List.member (ContPointer () FullContext) conflictPointers
                || highlightFullNode

        highlightVar var =
            highlightFullContext
                || List.member (ContPointer () (JustVarFromAssump var)) conflictPointers
                || List.member (ContPointer () (FullAssump var)) conflictPointers

        highlightColon var =
            highlightFullContext
                || List.member (ContPointer () (FullAssump var)) conflictPointers

        highlightType var =
            highlightFullContext
                || List.member (ContPointer () (FullAssump var)) conflictPointers
                || List.member (ContPointer () (JustTypFromAssump var)) conflictPointers

        typingAssumptionToHtmlList var typ =
            [ span []
                [ span [ classList [ ( "ruletree__conflict-highlight", highlightVar var ) ] ] [ text (contextHandler.showTermVar var) ]
                , span [ classList [ ( "ruletree__conflict-highlight", highlightColon var ) ] ] [ text ":" ]
                , span [ classList [ ( "ruletree__conflict-highlight", highlightType var ) ] ] [ text (contextHandler.showTypeForView typ viewLatinChar) ]
                ]
            ]
    in
    Dict.foldr
        (\var typ spanList ->
            typingAssumptionToHtmlList var typ |> (++) spanList
        )
        []
        dict
        |> List.intersperse (span [ classList [ ( "ruletree__conflict-highlight", highlightFullContext ) ] ] [ text ", " ])
        |> (\list ->
                if List.length list == 0 && not highlightFullContext then
                    [ span [ classList [ ( "ruletree__conflict-highlight", highlightFullNode ) ] ] [ text "⊢ " ] ]

                else if List.length list == 0 then
                    [ span [ classList [ ( "ruletree__conflict-highlight", True ) ] ] [ text "<?>" ]
                    , span [] [ text " ⊢ " ]
                    ]

                else
                    list ++ [ span [ classList [ ( "ruletree__conflict-highlight", highlightFullNode ) ] ] [ text " ⊢ " ] ]
           )


{-| Views given `term` with given highlightening from `conflictElementsRaw`.
-}
viewTerm : Term -> List (APointer () contPointer TermPointer typePointer) -> List (Html Msg)
viewTerm term conflictElementsRaw =
    let
        conflictElements =
            discardNodeIds conflictElementsRaw

        highlightFullTerm =
            List.member (TermPointer () FullTerm) conflictElements
                || List.member (FullNode ()) conflictElements
                || List.member (TermAndType ()) conflictElements
    in
    if highlightFullTerm then
        [ span [ class "ruletree__conflict-highlight" ] [ text <| showTerm term ] ]

    else
        case term of
            Var _ ->
                [ text <| showTerm term ]

            Abs var mTerm ->
                let
                    highlightAbsVar =
                        List.member (TermPointer () AbsVar) conflictElements

                    highlightAbsBody =
                        List.member (TermPointer () AbsBody) conflictElements
                in
                [ text "(λ"
                , span [ classList [ ( "ruletree__conflict-highlight", highlightAbsVar ) ] ] [ text <| showTermVar var ]
                , text "."
                , span [ classList [ ( "ruletree__conflict-highlight", highlightAbsBody ) ] ] [ text <| showTerm mTerm ]
                , text ")"
                ]

            App mTerm nTerm ->
                let
                    highlightAppLeft =
                        List.member (TermPointer () AppLeft) conflictElements

                    highlightAppRight =
                        List.member (TermPointer () AppRight) conflictElements
                in
                [ text "("
                , span [ classList [ ( "ruletree__conflict-highlight", highlightAppLeft ) ] ] [ text <| showTerm mTerm ]
                , text " "
                , span [ classList [ ( "ruletree__conflict-highlight", highlightAppRight ) ] ] [ text <| showTerm nTerm ]
                , text ")"
                ]


{-| Views given `typ` with given highlightening from `conflictElements`.
Representation of type variables in latin or greek letters is defined
by `viewLatinChar`.
-}
viewType : SType -> List (APointer () contPointer termPointer TypePointer) -> Bool -> List (Html Msg)
viewType typ conflictElements viewLatinChars =
    let
        highlightFullTypeOrFullRule =
            List.member (TypePointer () FullType) conflictElements
                || List.member (FullNode ()) conflictElements
                || List.member (TermAndType ()) conflictElements

        highlightLeft =
            List.member (TypePointer () ArrLeft) conflictElements

        highlightRight =
            List.member (TypePointer () ArrRight) conflictElements
    in
    if highlightFullTypeOrFullRule then
        [ span [ class "ruletree__conflict-highlight" ] [ text <| showTypeForView typ viewLatinChars ] ]

    else
        case typ of
            Arrow left right ->
                [ text "("
                , span [ classList [ ( "ruletree__conflict-highlight", highlightLeft ) ] ] [ text <| showTypeForView left viewLatinChars ]
                , text "→"
                , span [ classList [ ( "ruletree__conflict-highlight", highlightRight ) ] ] [ text <| showTypeForView right viewLatinChars ]
                , text ")"
                ]

            _ ->
                [ text <| showTypeForView typ viewLatinChars ]


{-| Views the typing judgement as given through `context`, `term` and `typ`
with given highlightening from `conflictElementsRaw`.
Representation of type variables in latin or greek letters is defined
by `viewLatinChar`.
-}
viewRuleContent : SContext -> Term -> SType -> List Pointer -> Bool -> List (Html Msg)
viewRuleContent context term typ pointersToHighlightRaw viewLatinChar =
    let
        colon =
            case term of
                Var _ ->
                    ":"

                _ ->
                    " : "

        pointersToHighlight =
            discardNodeIds pointersToHighlightRaw

        highlightColon =
            List.member (FullNode ()) pointersToHighlight || List.member (TermAndType ()) pointersToHighlight
    in
    viewContext stlcContextHandler context pointersToHighlight viewLatinChar
        ++ viewTerm term pointersToHighlight
        ++ span [ classList [ ( "ruletree__conflict-highlight", highlightColon ) ] ] [ text colon ]
        :: viewType typ pointersToHighlight viewLatinChar


stlcContextHandler : SContextHandler
stlcContextHandler =
    AContextHandler showTermVar showTypeForView


{-| Encodes given `ruleTree` as a `String`.
-}
encodeRuleTreeAsString : RuleTree -> String
encodeRuleTreeAsString ruleTree =
    let
        encodeRuleContent context term typ =
            ("{" ++ showContext context ++ "}" |> String.replace " " "") ++ showTerm term ++ "$" ++ showType typ ++ "$"

        encodeBool bool =
            if bool then
                "T"

            else
                "F"
    in
    case ruleTree of
        RVar context term typ hasBeenApplied ->
            "_V_" ++ encodeRuleContent context term typ ++ encodeBool hasBeenApplied

        RAbs context term typ ruleTree1 ->
            "_AB_" ++ encodeRuleContent context term typ ++ encodeRuleTreeAsString ruleTree1

        RApp context term typ ruleTree1 ruleTree2 ->
            "_AP_" ++ encodeRuleContent context term typ ++ encodeRuleTreeAsString ruleTree1 ++ encodeRuleTreeAsString ruleTree2

        Hole ->
            "_H_"


{-| Shows the given `TermVar`.
-}
showTermVar : TermVar -> String
showTermVar =
    String.fromChar


{-| Shows the term with explicit parantheses being used around all
Abstraction and Application expressions.
-}
showTerm : Term -> String
showTerm term =
    let
        showTermWithOutmostParantheses term_ =
            case term_ of
                Var var ->
                    String.fromChar var

                Abs var subterm ->
                    "(λ" ++ String.fromChar var ++ "." ++ showTerm subterm ++ ")"

                App left right ->
                    "(" ++ showTerm left ++ " " ++ showTerm right ++ ")"

        termAsStringRaw =
            showTermWithOutmostParantheses term

        hasOutmostParantheses =
            String.startsWith "(" termAsStringRaw && String.endsWith ")" termAsStringRaw
    in
    if hasOutmostParantheses then
        termAsStringRaw
        -- |> String.dropLeft 1 |> String.dropRight 1

    else
        termAsStringRaw


{-| Shows the type with explicit parantheses being used around all Arrow expressions.
-}
showType : SType -> String
showType typ =
    case typ of
        BasicType basicType ->
            basicType

        Arrow left right ->
            "(" ++ showType left ++ "→" ++ showType right ++ ")"

        Untyped ->
            "?"


{-| Shows the type with explicit parantheses being used around all Arrow expressions.
In addition to the plain `showType` function, this function handles the conversion
from latin to greek letters in case given `showLatinChars` equals `True`.
-}
showTypeForView : SType -> Bool -> String
showTypeForView typ showLatinChars =
    let
        isSingletonLetter typVar =
            case String.toList typVar of
                [ singleton ] ->
                    Char.isAlpha singleton

                head :: tail ->
                    Char.isAlpha head && List.all (Char.isAlpha >> not) tail

                _ ->
                    False
    in
    case typ of
        BasicType basicType ->
            if isSingletonLetter basicType && not showLatinChars then
                String.map charLatinToGreekRepresentation basicType

            else
                basicType

        Arrow left right ->
            "(" ++ showTypeForView left showLatinChars ++ "→" ++ showTypeForView right showLatinChars ++ ")"

        Untyped ->
            "?"


{-| Shows the given `SContext` as a list of typing assumptions
of the form `variable:type`.
-}
showContext : SContext -> String
showContext (Context dict) =
    Dict.foldl (\var typ outputString -> showTermVar var ++ ":" ++ showType typ ++ ", " ++ outputString) "endIndicator" dict
        -- removes the comma when there is at least one element in the context
        |> String.replace ", endIndicator" ""
        -- removes the indicator when the context is empty
        |> String.replace "endIndicator" ""


{-| Views the variable inference rule _(Var)_.
-}
viewVarRule : Model -> Html Msg
viewVarRule model =
    div
        [ classList
            [ ( "menu__inference-rule", True )
            , ( "menu__inference-rule--selected", model.menuState == VarRule )
            ]
        , onClick <| ChangeState VarRule
        , style "margin-left" "47px"
        , style "margin-right" "37px"
        , title "Select Variable Rule"
        ]
        [ div [ style "margin-right" "39px" ]
            [ div [ class "inference-rule__text-style" ] [ text "x : σ ∈ Γ" ]
            , div [ class "inference-rule__seperation-line" ] [ text " ———————— (Var)" ]
            , div [ class "inference-rule__text-style" ] [ text "Γ ⊢ x : σ" ]
            ]
        ]


{-| Views the application inference rule _(App)_.
-}
viewApplicationRule : Model -> Html Msg
viewApplicationRule model =
    div
        [ classList [ ( "menu__inference-rule", True ), ( "menu__inference-rule--selected", model.menuState == AppRule ) ]
        , onClick <| ChangeState AppRule
        , style "margin-right" "1px"
        , title "Select Application Rule"
        ]
        [ div [ style "margin-right" "2px" ]
            [ div [ class "inference-rule__text-style" ] [ text "Γ ⊢ M : (σ → τ)   Γ ⊢ N : σ" ]
            , div [ class "inference-rule__seperation-line" ] [ text "  ————————————————— (App)" ]
            , div [ class "inference-rule__text-style" ] [ text "Γ ⊢ (M N) : τ" ]
            ]
        ]


{-| Views the abstraction inference rule _(Abs)_.
-}
viewAbstractionRule : Model -> Html Msg
viewAbstractionRule model =
    div
        [ classList [ ( "menu__inference-rule", True ), ( "menu__inference-rule--selected", model.menuState == AbsRule ) ]
        , onClick <| ChangeState AbsRule
        , style "margin-left" "4px"
        , title "Select Abstraction Rule"
        ]
        [ div [ style "margin-right" "22px" ]
            [ div [ class "inference-rule__text-style" ] [ text "Γ, x : σ ⊢ M : τ" ]
            , div [ class "inference-rule__seperation-line" ] [ text "  ———————————————— (Abs)" ]
            , div [ class "inference-rule__text-style" ] [ text "Γ ⊢ (λx.M) : (σ → τ)" ]
            ]
        ]


{-| Creates a new `RuleTree` based on the term form.

    RVar for a Var Term
    RAbs for an Abs Term
    RApp for an App Term

-}
createRuleTree : SContext -> Term -> SType -> RuleTree
createRuleTree context term typ =
    case term of
        Var _ ->
            RVar context term typ False

        Abs _ _ ->
            RAbs context term typ Hole

        App _ _ ->
            RApp context term typ Hole Hole


{-| Returns `True` iff all leafs are `RVar` nodes which have the inference rule applied by the user.
-}
ruleTreeIsComplete : RuleTree -> Bool
ruleTreeIsComplete ruleTree =
    case ruleTree of
        RVar _ _ _ hasBeenApplied ->
            hasBeenApplied

        RAbs _ _ _ childRuleTree ->
            ruleTreeIsComplete childRuleTree

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            ruleTreeIsComplete childRuleTree1 && ruleTreeIsComplete childRuleTree2

        Hole ->
            False


{-| Returns `True` iff given `ruleTree` is complete (see function `ruleTreeIsComplete`),
there do not exist any conflicts (see `getConflictsInRuleTree`), and all FV exist
in its context (see `checkIfAllFreeVariablesExistInRuleTreesContext`).
-}
ruleTreeIsSuccessful : RuleTree -> List Pointer -> Bool
ruleTreeIsSuccessful ruleTree conflictPointers =
    ruleTreeIsComplete ruleTree
        && List.isEmpty conflictPointers
        && allFreeVariablesExistInRuleTreesContext ruleTree


{-| Returns a Display Message iff given `ruleTree` is correct and complete but
there is at least one free variable missing in its context.
-}
generateDisplayMessageIfRuleTreeIsSuccessfulBesidesContextMissingFreeVar : RuleTree -> List Pointer -> Maybe String
generateDisplayMessageIfRuleTreeIsSuccessfulBesidesContextMissingFreeVar ruleTree conflictPointers =
    if
        ruleTreeIsComplete ruleTree
            && List.isEmpty conflictPointers
            && (not <| allFreeVariablesExistInRuleTreesContext ruleTree)
    then
        Just "The root term contains free variables that must exist in all contexts!"

    else
        Nothing


{-| Returns all free variables (FV) from the term of given `ruleTree`.
In case of `ruleTree == Hole` it will simply return a Set with `Var "x"`
as we currently don't need any special handling for function calls
on `Hole`.
-}
getFreeVariablesFromRuleTreesTerm : RuleTree -> Set TermVar
getFreeVariablesFromRuleTreesTerm ruleTree =
    let
        term =
            getTermFromRuleTree ruleTree |> Maybe.withDefault (Var 'x')

        collectAllVar allVar term_ =
            case term_ of
                Var var ->
                    Set.insert var allVar

                Abs var body ->
                    collectAllVar (Set.insert var allVar) body

                App leftTerm rightTerm ->
                    Set.union (collectAllVar allVar leftTerm) (collectAllVar allVar rightTerm)

        collectBoundVar boundVar term_ =
            case term_ of
                Var _ ->
                    boundVar

                Abs var body ->
                    collectBoundVar (Set.insert var boundVar) body

                App leftTerm rightTerm ->
                    Set.union (collectBoundVar boundVar leftTerm) (collectBoundVar boundVar rightTerm)
    in
    Set.diff (collectAllVar Set.empty term) (collectBoundVar Set.empty term)


{-| Checks for given `ruleTree` if all its term's FV exist in its context.
-}
allFreeVariablesExistInRuleTreesContext : RuleTree -> Bool
allFreeVariablesExistInRuleTreesContext ruleTree =
    let
        freeVarSet =
            getFreeVariablesFromRuleTreesTerm ruleTree

        contextDict =
            case getContextFromRuleTree ruleTree of
                Context dict ->
                    dict
    in
    Set.filter (\freeVar -> Dict.member freeVar contextDict) freeVarSet
        |> Set.size
        |> (==) (Set.size freeVarSet)


{-| Checks for given `term` if variable shadowing is occuring.
-}
variableShadowingIsOccuring : Term -> Set TermVar -> Bool
variableShadowingIsOccuring term setOfFoundBoundVarSoFar =
    case term of
        Var _ ->
            False

        Abs var body ->
            Set.member var setOfFoundBoundVarSoFar
                || variableShadowingIsOccuring body (Set.insert var setOfFoundBoundVarSoFar)

        App leftTerm rightTerm ->
            variableShadowingIsOccuring leftTerm setOfFoundBoundVarSoFar
                || variableShadowingIsOccuring rightTerm setOfFoundBoundVarSoFar
