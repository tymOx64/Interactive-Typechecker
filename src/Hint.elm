module Hint exposing (..)

import SharedStructures exposing (..)
import SimplyTypedLambdaCalculus exposing (..)



{- getAllAssociatedRuleTreeElementsUpwards : List Int -> RuleTree -> RuleElement -> List RuleElementEntity
   getAllAssociatedRuleTreeElementsUpwards nodeId ruleTree ruleElement =
       case ruleTree of
           RVar _ _ _ _ ->
               [ ( nodeId, ruleElement ) ]

           RAbs _ (Abs xVar mTerm) _ nextRuleTree ->
               ( nodeId, ruleElement )
                   :: (case ruleElement of
                           FullContext ->
                               getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree FullContext

                           TypingAssumptionFull var ->
                               getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree (TypingAssumptionFull var)

                           MTerm ->
                               getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree FullTerm

                           AbsXVar ->
                               getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree (TypingAssumptionJustVar xVar)

                           _ ->
                               []
                      )

           _ ->
               []
-}
{- type RuleElement
   = FullRule
   | FullContext
   | TypingAssumptionForVar Var
   | TypingAssumptionJustVar Var
   | TypingAssumptionJustTypeForVar Var
   | TypingRelation
   | FullTerm
   | AbsXVar
   | MTerm
   | NTerm
   | FullType
   | ArrowRightType
   | ArrowLeftType
-}
{- groupAllAssociatedTypeEntities : RuleTree -> List Int -> Maybe RuleElement -> List (List RuleElementEntity)
   groupAllAssociatedTypeEntities ruleTree nodeId ruleElement =
       case ruleElement of
           Nothing ->
               case ruleTree of
                   RVar _ _ _ _ ->
                       [ [ ( nodeId, ruleElement ) ] ]

                   RAbs _ (Abs xVar mTerm) _ nextRuleTree ->
                       ( nodeId, ruleElement )
                           :: (case ruleElement of
                                   FullContext ->
                                       getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree FullContext

                                   TypingAssumptionFull var ->
                                       getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree (TypingAssumptionFull var)

                                   MTerm ->
                                       getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree FullTerm

                                   AbsXVar ->
                                       getAllAssociatedRuleTreeElementsUpwards (nodeId ++ [ 0 ]) nextRuleTree (TypingAssumptionJustVar xVar)

                                   _ ->
                                       []
                              )

                   _ ->
                       []

           _ ->
               []
-}
{- getIdToTypeDictForRuleTree : RuleTree -> Dict Int SType
   getIdToTypeDictForRuleTree ruleTree =
       case ruleTree of
           RVar context term type1 ->
               .

           RAbs context term type1 ruleTree1 ->
               .

           RApp context term type1 ruleTree1 ruleTree2 ->
               .

           Undetermined context term type1 ->
               .
-}
{- getHintsForSelectedRuleTree model =
   let
       selectedRuleTree =
           STLC.getSelectedRuleTreeNode model
   in
   case ( selectedRuleTree, model.menuState ) of
       ( ( RVar context term type1 ), MenuState.VarRule ) ->
           5

       ( ( RAbs context term type1 ruleTree1 ), MenuState.AbsRule ) ->
           5

       ( ( RApp context term type1 ruleTree1 ruleTree2, MenuState.AppRule ) ->
           5

       ( ( Undetermined context term type1 ), MenuState.Und ->
           5
-}


determineCorrespondingRule : Term -> MenuState
determineCorrespondingRule term =
    case term of
        Var _ ->
            VarRule

        Abs _ _ ->
            AbsRule

        App _ _ ->
            AppRule
