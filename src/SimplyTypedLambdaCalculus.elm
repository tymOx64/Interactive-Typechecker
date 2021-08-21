module SimplyTypedLambdaCalculus exposing (..)

--exposing (RuleTree, SContext, Term, SType, Type, Context, Var, r00, showRules, showTerm, showRuleContent, t1)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (dict)
import SharedStructures as Shared exposing (..)


viewRuleTree : RuleTree -> List Int -> Model -> List Pointer -> Html Msg
viewRuleTree ruleTree nodeId model pointersToHighlight =
    let
        onClickSelect =
            onClick <| SelectTreeNode <| nodeId

        nodeAttributes =
            [ classList [ ( "conclusion", True ), ( "conclusion--selected", nodeId == model.selectedNodeId ) ]
            , onClickSelect
            , onDoubleClick <| ResetTreeNode <| nodeId
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
                    if hasBeenApplied then
                        text ""

                    else
                        div [ class "premise", class "hole", onClickSelect ] [ text "?" ]
            in
            div [ class "rule" ]
                [ premise
                , div [ class "line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree)
                ]

        RAbs context term typ nextRuleTree ->
            let
                premise =
                    if nextRuleTree == Hole then
                        div [ class "premise", class "hole", onClickSelect ] [ text "?" ]

                    else
                        div [ class "premise" ] [ viewRuleTree nextRuleTree (nodeId ++ [ 0 ]) model pointersToHighlight ]
            in
            div [ class "rule" ]
                [ premise
                , div [ class "line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree)
                ]

        RApp context term typ nextRuleTree1 nextRuleTree2 ->
            let
                premise =
                    if nextRuleTree1 == Hole && nextRuleTree2 == Hole then
                        div [ class "premise", class "hole", onClickSelect ] [ text "?" ]

                    else
                        div [ class "premise" ]
                            [ div [ class "leftSide" ]
                                [ viewRuleTree nextRuleTree1 (nodeId ++ [ 0 ]) model pointersToHighlight ]
                            , div [ class "leftSide" ]
                                [ viewRuleTree nextRuleTree2 (nodeId ++ [ 1 ]) model pointersToHighlight ]
                            ]
            in
            div [ class "rule" ]
                [ premise
                , div [ class "line" ] []
                , div nodeAttributes (viewRuleContent context term typ pointersForCurrentRuleTree)
                ]

        Hole ->
            div [ class "rule" ]
                [ text "?" ]


isLeaf : RuleTree -> Bool
isLeaf ruleTree =
    case ruleTree of
        RAbs _ _ _ nextRuleTree ->
            nextRuleTree == Hole

        RApp _ _ _ nextRuleTree1 nextRuleTree2 ->
            nextRuleTree1 == Hole && nextRuleTree2 == Hole

        _ ->
            True


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
                (termAndTypeConflictsExistingTypingAssumption var typ context)
                [ [ ContPointer nodeId (FullAssump var), TermAndType nodeId ] ]
                ++ appendIfConditionHolds
                    (termAndTypeMissesTypingAssumption var context)
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

        RAbs context (Abs var mTerm) (Shared.Arrow sigma tau) nextRuleTree ->
            let
                removeVarFromContext (Context dict) =
                    Context <| Dict.remove var dict

                nextContextWithOutAbstractionVar =
                    removeVarFromContext (getContextFromRuleTree nextRuleTree)

                sigmaIsConflicting =
                    Maybe.map2 (/=) (Just sigma) (getTypeFromContext var (getContextFromRuleTree nextRuleTree))
                        |> Maybe.withDefault True

                tauIsConflicting =
                    Maybe.map2 (/=) (Just tau) (getTermTypeFromRuleTree nextRuleTree)
                        |> Maybe.withDefault True

                contextIsConflicting =
                    not <| contextsAreEqual context nextContextWithOutAbstractionVar

                mTermIsConflicting =
                    getTermFromRuleTree nextRuleTree |> Maybe.andThen (\term -> Just <| term /= mTerm) |> Maybe.withDefault False
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
                ++ getConflictsInRuleTree nextRuleTree (nodeId ++ [ 0 ])

        RAbs _ _ (Arrow _ _) nextRuleTree ->
            [ TermPointer nodeId FullTerm ] :: getConflictsInRuleTree nextRuleTree (nodeId ++ [ 0 ])

        RAbs _ (Abs _ _) _ nextRuleTree ->
            [ TypePointer nodeId FullType ] :: getConflictsInRuleTree nextRuleTree (nodeId ++ [ 0 ])

        RAbs _ _ _ nextRuleTree ->
            [ [ TermPointer nodeId FullTerm ], [ TypePointer nodeId FullType ] ] ++ getConflictsInRuleTree nextRuleTree (nodeId ++ [ 0 ])

        RApp _ (App _ _) _ Hole Hole ->
            []

        RApp _ _ _ Hole Hole ->
            [ [ TermPointer nodeId FullTerm ] ]

        RApp context (App ((Abs _ _) as mTerm) nTerm) tau nextRuleTree1 nextRuleTree2 ->
            let
                tauIsConflicting =
                    tau /= getTauTypeFromAbsRuleTree nextRuleTree1

                mTermIsConflicting =
                    getTermFromRuleTree nextRuleTree1 |> Maybe.andThen (\nextMTerm -> Just <| nextMTerm /= mTerm) |> Maybe.withDefault False

                nTermIsConflicting =
                    getTermFromRuleTree nextRuleTree2 |> Maybe.andThen (\nextNTerm -> Just <| nextNTerm /= nTerm) |> Maybe.withDefault False

                leftContextIsConflicting =
                    not <| contextsAreEqual context <| getContextFromRuleTree nextRuleTree1

                rightContextIsConflicting =
                    not <| contextsAreEqual context <| getContextFromRuleTree nextRuleTree2

                upperSigmaIsConflicting =
                    Maybe.map2 (/=) (getSigmaTypeFromAbsRuleTree nextRuleTree1) (getTermTypeFromRuleTree nextRuleTree2)
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
                ++ getConflictsInRuleTree nextRuleTree1 (nodeId ++ [ 0 ])
                ++ getConflictsInRuleTree nextRuleTree2 (nodeId ++ [ 1 ])

        RApp _ _ _ _ _ ->
            [ [ TermPointer nodeId AppLeft ] ]

        Hole ->
            []


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



{- contextSubsetsContext : SContext -> SContext -> Bool
   contextSubsetsContext (Context dict1) (Context dict2) =
       let
           varTypePairIsContainedByDict2 var1 typ1 =
               Dict.get var1 dict2 |> Maybe.andThen (\typ2 -> Just (typ1 == typ2)) |> Maybe.withDefault False
       in
       Dict.foldl (\var1 typ1 isSubset -> varTypePairIsContainedByDict2 var1 typ1 |> (&&) isSubset) True dict1
-}


{-| Checks for given contexts if they are equal, i.e. all key-value-pairs appear in both contexts.
-}
contextsAreEqual : SContext -> SContext -> Bool
contextsAreEqual (Context dict1) (Context dict2) =
    Dict.union dict1 dict2 |> (==) dict2


termAndTypeMissesTypingAssumption : Var -> SContext -> Bool
termAndTypeMissesTypingAssumption var (Context dict) =
    Dict.foldl
        (\varFromContext _ typingAssumptionIsMissing ->
            if typingAssumptionIsMissing then
                var /= varFromContext

            else
                False
        )
        True
        dict


{-| Checks for `RVar` rules if the variable and its type conflict
with an existing typing assumption in the context.
Calling this function on the following rules would return:

        x:α, y:β ⊢ x:γ => True
        x:α, y:β ⊢ x:α => False
        y:β ⊢ x:γ => False

As the last example shows the function always returns `False` if the according
typing assumption is missing. If you need to check for that, use `termAndTypeMissesTypingAssumption`.

-}
termAndTypeConflictsExistingTypingAssumption : Var -> SType -> SContext -> Bool
termAndTypeConflictsExistingTypingAssumption var typ (Context dict) =
    Dict.foldl
        (\varFromContext typFromContext conflictFound ->
            if conflictFound then
                True

            else
                (var == varFromContext) && (typ /= typFromContext)
        )
        False
        dict


getContextFromRuleTree : RuleTree -> SContext
getContextFromRuleTree ruleTree =
    case ruleTree of
        RVar context _ _ _ ->
            context

        RAbs context _ _ _ ->
            context

        RApp context _ _ _ _ ->
            context

        _ ->
            Context Dict.empty


getTermFromRuleTree : RuleTree -> Maybe Term
getTermFromRuleTree ruleTree =
    case ruleTree of
        RVar _ term _ _ ->
            Just term

        RAbs _ term _ _ ->
            Just term

        RApp _ term _ _ _ ->
            Just term

        _ ->
            Nothing


getTermTypeFromRuleTree : RuleTree -> Maybe SType
getTermTypeFromRuleTree ruleTree =
    case ruleTree of
        RVar _ _ typ _ ->
            Just typ

        RAbs _ _ typ _ ->
            Just typ

        RApp _ _ typ _ _ ->
            Just typ

        Hole ->
            Nothing


getSigmaTypeFromAbsRuleTree : RuleTree -> Maybe SType
getSigmaTypeFromAbsRuleTree ruleTree =
    case ruleTree of
        RAbs _ _ (Arrow sigma _) _ ->
            Just sigma

        _ ->
            Nothing


getTauTypeFromAbsRuleTree : RuleTree -> SType
getTauTypeFromAbsRuleTree ruleTree =
    case ruleTree of
        RAbs _ _ (Shared.Arrow _ tau) _ ->
            tau

        _ ->
            Untyped


getTypeFromContext : Var -> SContext -> Maybe SType
getTypeFromContext var (Context dict) =
    Dict.get var dict


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
        ( RAbs a b c nextRuleTree, 0 :: nextNodeId ) ->
            RAbs a b c <| changeRuleTreeNode nextRuleTree nextNodeId newRuleTree keepOldChildren

        ( RApp a b c nextRuleTree d, 0 :: nextNodeId ) ->
            (RApp a b c <| changeRuleTreeNode nextRuleTree nextNodeId newRuleTree keepOldChildren) d

        ( RApp a b c d nextRuleTree, 1 :: nextNodeId ) ->
            RApp a b c d <| changeRuleTreeNode nextRuleTree nextNodeId newRuleTree keepOldChildren

        ( _, [] ) ->
            newRuleTree

        --der letzte fall fängt den vorletzten natürlich immer mit ab, jedoch dürfte der unterste fall nur als ein fehler auftreten, daher diesen ggf später noch gesondert behandeln
        ( _, _ ) ->
            newRuleTree


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


getRuleTreeNode : RuleTree -> List Int -> RuleTree
getRuleTreeNode ruleTree nodeId =
    case ( ruleTree, nodeId ) of
        ( RAbs _ _ _ nextRuleTree, 0 :: nextNodeId ) ->
            getRuleTreeNode nextRuleTree nextNodeId

        ( RApp _ _ _ nextRuleTree _, 0 :: nextNodeId ) ->
            getRuleTreeNode nextRuleTree nextNodeId

        ( RApp _ _ _ _ nextRuleTree, 1 :: nextNodeId ) ->
            getRuleTreeNode nextRuleTree nextNodeId

        ( _, [] ) ->
            ruleTree

        ( _, _ ) ->
            Hole


getSelectedRuleTreeNode : Model -> RuleTree
getSelectedRuleTreeNode model =
    getRuleTreeNode model.ruleTree model.selectedNodeId


showNodeId : List Int -> String
showNodeId nodeId =
    List.foldl (\int str -> str ++ String.fromInt int) "" nodeId


{-| Returns the target node id if there is one available, otherwise returns the currently selected node id (i.e. the selected node id remains unchanged).
The direction is defined through the parameter isLeftKeyEvent.
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

        RAbs _ _ _ nextRuleTree ->
            ruleTreeAsInOrderList nextRuleTree (nodeId ++ [ 0 ]) (level + 1) currentUserSelectedNodeId |> (++) [ ( nodeId, level ) ]

        RApp _ _ _ nextRuleTree1 nextRuleTree2 ->
            ruleTreeAsInOrderList nextRuleTree1 (nodeId ++ [ 0 ]) (level + 1) currentUserSelectedNodeId
                |> (++) [ ( nodeId, level ) ]
                |> (++) (ruleTreeAsInOrderList nextRuleTree2 (nodeId ++ [ 1 ]) (level + 1) currentUserSelectedNodeId)

        Hole ->
            []


{-| Keeps the children of the children (_2nd generation children_) if the tree structure **remains unchanged**.
The newly generated _1st generation children_ **remain new**. But the old 2nd generation children remain old if the
tree structure allows for that. If the tree structure doesn't allow for that, the 2nd generation gets removed and
all subsequent generation of the newRuleTree are being kept (which is usually just the one coming from the premise of the inference rule).

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



{- showRuleContent : SContext -> Term -> SType -> String
   showRuleContent context term type1 =
       case term of
           Var _ ->
               showContext context ++ showTerm term ++ ":" ++ showType type1

           _ ->
               showContext context ++ showTerm term ++ " : " ++ showType type1
-}


viewContext :
    AContextHandler comparable typ
    -> AContext comparable typ
    -> List (APointer () (AContPointer comparable) termPointer typePointer)
    -> List (Html Msg)
viewContext contextHandler (Context dict) conflictPointers =
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
                [ span [ classList [ ( "ruletree-text-highlight", highlightVar var ) ] ] [ text (contextHandler.showVar var) ]
                , span [ classList [ ( "ruletree-text-highlight", highlightColon var ) ] ] [ text ":" ]
                , span [ classList [ ( "ruletree-text-highlight", highlightType var ) ] ] [ text (contextHandler.showType typ) ]
                ]
            ]
    in
    Dict.foldr
        (\var typ spanList ->
            typingAssumptionToHtmlList var typ |> (++) spanList
        )
        []
        dict
        |> List.intersperse (span [ classList [ ( "ruletree-text-highlight", highlightFullContext ) ] ] [ text ", " ])
        |> (\list ->
                if List.length list == 0 && not highlightFullContext then
                    [ span [ classList [ ( "ruletree-text-highlight", highlightFullNode ) ] ] [ text "⊢ " ] ]

                else if List.length list == 0 then
                    [ span [ classList [ ( "ruletree-text-highlight", True ) ] ] [ text "<?>" ]
                    , span [] [ text " ⊢ " ]
                    ]

                else
                    list ++ [ span [ classList [ ( "ruletree-text-highlight", highlightFullNode ) ] ] [ text " ⊢ " ] ]
           )


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
        [ span [ class "ruletree-text-highlight" ] [ text <| showTerm term ] ]

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
                , span [ classList [ ( "ruletree-text-highlight", highlightAbsVar ) ] ] [ text <| showVar var ]
                , text "."
                , span [ classList [ ( "ruletree-text-highlight", highlightAbsBody ) ] ] [ text <| showTerm mTerm ]
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
                , span [ classList [ ( "ruletree-text-highlight", highlightAppLeft ) ] ] [ text <| showTerm mTerm ]
                , text " "
                , span [ classList [ ( "ruletree-text-highlight", highlightAppRight ) ] ] [ text <| showTerm nTerm ]
                , text ")"
                ]


viewType : SType -> List (APointer () contPointer termPointer TypePointer) -> List (Html Msg)
viewType typ conflictElements =
    let
        highlightFullTypeOrFullRule =
            List.member (TypePointer () FullType) conflictElements
                || List.member (FullNode ()) conflictElements

        highlightLeft =
            List.member (TypePointer () ArrLeft) conflictElements

        highlightRight =
            List.member (TypePointer () ArrRight) conflictElements
    in
    if highlightFullTypeOrFullRule then
        [ span [ class "ruletree-text-highlight" ] [ text <| showType typ ] ]

    else
        case typ of
            Arrow left right ->
                [ text "("
                , span [ classList [ ( "ruletree-text-highlight", highlightLeft ) ] ] [ text <| showType left ]
                , text "→"
                , span [ classList [ ( "ruletree-text-highlight", highlightRight ) ] ] [ text <| showType right ]
                , text ")"
                ]

            _ ->
                [ text <| showType typ ]


viewRuleContent : SContext -> Term -> SType -> List Pointer -> List (Html Msg)
viewRuleContent context term typ pointersToHighlightRaw =
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
    viewContext stlcContextHandler context pointersToHighlight
        ++ viewTerm term pointersToHighlight
        ++ span [ classList [ ( "ruletree-text-highlight", highlightColon ) ] ] [ text colon ]
        :: viewType typ pointersToHighlight


stlcContextHandler : AContextHandler Var (Type Char)
stlcContextHandler =
    AContextHandler showVar showType


addTypingAssumptionToContext : Var -> SType -> SContext -> SContext
addTypingAssumptionToContext var typ (Context dict) =
    Context <| Dict.insert var typ dict


encodeRuleTreeAsString : RuleTree -> String
encodeRuleTreeAsString ruleTree =
    let
        encodeRuleContent context term typ =
            ("{" ++ showContext context ++ "}" |> String.replace " " "") ++ showTerm term ++ showType typ

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


showTerm : Term -> String
showTerm term =
    case term of
        Var var ->
            String.fromChar var

        Abs var subterm ->
            "(λ" ++ showVar var ++ "." ++ showTerm subterm ++ ")"

        App subterm1 subterm2 ->
            "(" ++ showTerm subterm1 ++ " " ++ showTerm subterm2 ++ ")"


showVar : Char -> String
showVar var =
    String.fromChar var


showType : Type Char -> String
showType type1 =
    case type1 of
        BasicType basicType ->
            showVar basicType

        Arrow basicType1 basicType2 ->
            "(" ++ showType basicType1 ++ "→" ++ showType basicType2 ++ ")"

        Untyped ->
            "?"


showContext : SContext -> String
showContext (Context dict) =
    Dict.foldl (\var type1 outputString -> showVar var ++ ":" ++ showType type1 ++ ", " ++ outputString) "endIndicator" dict
        |> String.replace ", endIndicator" ""
        |> String.replace "endIndicator" ""


isEmptyContext : AContext var typ -> Bool
isEmptyContext (Context dict) =
    Dict.isEmpty dict


showRules : Term -> String
showRules term =
    case term of
        Var _ ->
            "Axiom, "

        Abs _ subterm ->
            "Abstraction, " ++ showRules subterm

        App subterm1 subterm2 ->
            "Application, " ++ showRules subterm1 ++ showRules subterm2


viewVarRule : Model -> Html Msg
viewVarRule model =
    div [ classList [ ( "menuRule", True ), ( "menuRule--selected", model.menuState == VarRule ) ], onClick <| ChangeState VarRule, style "margin-left" "47px", style "margin-right" "37px" ]
        [ div [ classList [], style "margin-right" "39px" ]
            [ div [ class "menuText" ] [ text "x : σ ∈ Γ" ]
            , div [ class "menuLine" ] [ text " ———————— (Var)" ]
            , div [ class "menuText" ] [ text "Γ ⊢ x : σ" ]
            ]
        ]


viewApplicationRule : Model -> Html Msg
viewApplicationRule model =
    div [ classList [ ( "menuRule", True ), ( "menuRule--selected", model.menuState == AppRule ) ], onClick <| ChangeState AppRule, style "margin-right" "1px" ]
        [ div [ style "margin-right" "2px" ]
            [ div [ class "menuText" ] [ text "Γ ⊢ M : (σ → τ)   Γ ⊢ N : σ" ]
            , div [ class "menuLine" ] [ text "  ————————————————— (App)" ]
            , div [ class "menuText" ] [ text "Γ ⊢ (M N) : τ" ]
            ]
        ]


viewAbstractionRule : Model -> Html Msg
viewAbstractionRule model =
    div [ classList [ ( "menuRule", True ), ( "menuRule--selected", model.menuState == AbsRule ) ], onClick <| ChangeState AbsRule, style "margin-left" "4px" ]
        [ div [ style "margin-right" "22px" ]
            [ div [ class "menuText" ] [ text "Γ, x : σ ⊢ M : τ" ]
            , div [ class "menuLine" ] [ text "  ———————————————— (Abs)" ]
            , div [ class "menuText" ] [ text "Γ ⊢ (λx.M) : (σ → τ)" ]
            ]
        ]


createRuleTree : SContext -> Term -> SType -> RuleTree
createRuleTree context term typ =
    case term of
        Var _ ->
            RVar context term typ False

        Abs _ _ ->
            RAbs context term typ Hole

        App _ _ ->
            RApp context term typ Hole Hole


determineCorrespondingRule : Term -> MenuState
determineCorrespondingRule term =
    case term of
        Var _ ->
            VarRule

        Abs _ _ ->
            AbsRule

        App _ _ ->
            AppRule
