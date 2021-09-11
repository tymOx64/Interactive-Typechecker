module RuleTreeUtils exposing (..)

import Dict
import SharedStructures exposing (AContext(..), RuleTree(..), SContext, SType, Term(..), Type(..))


isLeaf : RuleTree -> Bool
isLeaf ruleTree =
    case ruleTree of
        RAbs _ _ _ childRuleTree ->
            childRuleTree == Hole

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            childRuleTree1 == Hole && childRuleTree2 == Hole

        _ ->
            True


getRuleTreeNode : RuleTree -> List Int -> RuleTree
getRuleTreeNode ruleTree nodeId =
    case ( ruleTree, nodeId ) of
        ( RAbs _ _ _ childRuleTree, 0 :: childNodeId ) ->
            getRuleTreeNode childRuleTree childNodeId

        ( RApp _ _ _ childRuleTree _, 0 :: childNodeId ) ->
            getRuleTreeNode childRuleTree childNodeId

        ( RApp _ _ _ _ childRuleTree, 1 :: childNodeId ) ->
            getRuleTreeNode childRuleTree childNodeId

        ( _, [] ) ->
            ruleTree

        ( _, _ ) ->
            Hole


{-| Returns the context from given `ruleTree`
-}
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

        Hole ->
            Nothing


{-| Returns the type of the term from given `ruleTree`.
-}
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


{-| Returns the left type of the term from given `ruleTree`.
-}
getLeftTypeFromRuleTree : RuleTree -> Maybe SType
getLeftTypeFromRuleTree ruleTree =
    case getTermTypeFromRuleTree ruleTree of
        Just (Arrow left _) ->
            Just left

        _ ->
            Nothing


{-| Returns the right type of the term from given `ruleTree`.
-}
getRightTypeFromRuleTree : RuleTree -> Maybe SType
getRightTypeFromRuleTree ruleTree =
    case getTermTypeFromRuleTree ruleTree of
        Just (Arrow _ right) ->
            Just right

        _ ->
            Nothing
