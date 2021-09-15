module RuleTreeUtils exposing (..)

import Dict
import SharedStructures exposing (AContext(..), AType(..), Model, RuleTree(..), SContext, SType, Term(..), TermVar)


{-| Returns `True` iff given `ruleTree` is a leaf, i.e. does not have any child nodes.
-}
isLeaf : RuleTree -> Bool
isLeaf ruleTree =
    case ruleTree of
        RAbs _ _ _ childRuleTree ->
            childRuleTree == Hole

        RApp _ _ _ childRuleTree1 childRuleTree2 ->
            childRuleTree1 == Hole && childRuleTree2 == Hole

        _ ->
            True


{-| Returns the `RuleTree` at given `nodeID`.
The argument for `ruleTree` is supposed to be the root-`RuleTree`.
-}
getRuleTreeNode : RuleTree -> List Int -> RuleTree
getRuleTreeNode ruleTree nodeID =
    case ( ruleTree, nodeID ) of
        ( RAbs _ _ _ childRuleTree, 0 :: childNodeID ) ->
            getRuleTreeNode childRuleTree childNodeID

        ( RApp _ _ _ childRuleTree _, 0 :: childNodeID ) ->
            getRuleTreeNode childRuleTree childNodeID

        ( RApp _ _ _ _ childRuleTree, 1 :: childNodeID ) ->
            getRuleTreeNode childRuleTree childNodeID

        ( _, [] ) ->
            ruleTree

        ( _, _ ) ->
            Hole


{-| Returns the ruletree that is currently selected by the user.
-}
getSelectedRuleTreeNode : Model -> RuleTree
getSelectedRuleTreeNode model =
    getRuleTreeNode model.ruleTree model.selectedNodeID


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


{-| Returns the `Term` from given `ruleTree`.
-}
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


{-| Adds the typing assumption given through `var` and `typ` to given `Context`.
-}
addTypingAssumptionToContext : TermVar -> SType -> SContext -> SContext
addTypingAssumptionToContext var typ (Context dict) =
    Context <| Dict.insert var typ dict
