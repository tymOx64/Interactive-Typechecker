module SharedStructures exposing (..)

import Dict exposing (Dict)


type alias Model =
    { menuState : MenuState
    , ruleTree : RuleTree
    , selectedNodeId : List Int
    , latestTermVarTypings : Dict TermVar SType
    , gammaInput : String
    , xInput : String
    , mInput : String
    , nInput : String
    , sigmaInput : String
    , tauInput : String
    , displayMessage : String
    , ruleTreeSuccessful : Bool
    , baseUrl : String
    , viewLatinChar : Bool
    }


type Msg
    = Gamma String
    | X String
    | M String
    | N String
    | Sigma String
    | Tau String
    | Hint InputKind
    | TransformInput
    | FillAllInputs
    | FlushAllInputs
    | ApplyLatestChangesToFullRuleTree
    | Apply
    | SelectTreeNode (List Int)
    | ResetTreeNode (List Int)
    | ChangeState MenuState
    | KeyDown String
    | UrlChanged String
    | Start
    | GetUrl
    | ToggleLatinView
    | StartPage
    | NoOperation



-- STLC


type alias TermVar =
    Char


type Type basicType
    = BasicType basicType
    | Arrow (Type basicType) (Type basicType)
    | Untyped


type alias SType =
    Type String


type AContext termVar typ
    = Context (Dict termVar typ)


type alias SContext =
    AContext TermVar SType


type Term
    = Var TermVar
    | Abs TermVar Term
    | App Term Term


type RuleTree
    = RVar SContext Term SType Bool
    | RAbs SContext Term SType RuleTree
    | RApp SContext Term SType RuleTree RuleTree
    | Hole


type InputKind
    = GammaInput
    | XInput
    | MInput
    | NInput
    | SigmaInput
    | TauInput
    | RuleSelection


type APointer nodeId contPointer termPointer typePointer
    = FullNode nodeId
    | TermAndType nodeId
    | ContPointer nodeId contPointer
    | TermPointer nodeId termPointer
    | TypePointer nodeId typePointer


containsFullNode : List (APointer nodeId contPointer termPointer typePointer) -> Bool
containsFullNode =
    List.foldl
        (\pointer fullNodeFound ->
            fullNodeFound
                || (case pointer of
                        FullNode _ ->
                            True

                        _ ->
                            False
                   )
        )
        False


containsFullContext : List (APointer nodeId (AContPointer var) termPointer typePointer) -> Bool
containsFullContext =
    List.foldl
        (\pointer fullNodeFound ->
            fullNodeFound
                || (case pointer of
                        ContPointer _ FullContext ->
                            True

                        _ ->
                            False
                   )
        )
        False


fmapPointer :
    (nodeId -> newNodeId)
    -> (contPointer -> newContPointer)
    -> (termPointer -> newTermPointer)
    -> (typePointer -> newTypePointer)
    -> APointer nodeId contPointer termPointer typePointer
    -> APointer newNodeId newContPointer newTermPointer newTypePointer
fmapPointer nodeIdFunc contPointerFunc termPointerFunc typePointerFunc aPointer =
    case aPointer of
        FullNode nodeId ->
            FullNode (nodeIdFunc nodeId)

        TermAndType nodeId ->
            TermAndType (nodeIdFunc nodeId)

        ContPointer nodeId contPointer ->
            ContPointer (nodeIdFunc nodeId) (contPointerFunc contPointer)

        TermPointer nodeId termPointer ->
            TermPointer (nodeIdFunc nodeId) (termPointerFunc termPointer)

        TypePointer nodeId typePointer ->
            TypePointer (nodeIdFunc nodeId) (typePointerFunc typePointer)


discardNodeIds : List (APointer nodeId contPointer termPointer typePointer) -> List (APointer () contPointer termPointer typePointer)
discardNodeIds =
    List.map <| fmapPointer (\_ -> ()) identity identity identity


type alias Pointer =
    APointer (List Int) ContPointer TermPointer TypePointer


type AContPointer var
    = FullContext
    | FullAssump var
    | JustVarFromAssump var
    | JustTypFromAssump var



{- type alias AContextHandler t var typ =
   { t
       | showVar : var -> String
       , showType : typ -> String
   }
-}


type alias AContextHandler termVar typ =
    { showTermVar : termVar -> String
    , showTypeForView : typ -> Bool -> String
    }


type alias SContextHandler =
    AContextHandler TermVar SType


type alias ContPointer =
    AContPointer TermVar


type TypePointer
    = FullType
    | ArrLeft
    | ArrRight


type TermPointer
    = FullTerm
    | AbsVar
    | AbsBody
    | AppLeft
    | AppRight


getNodeIdFromPointer : APointer nodeId contPointer termPointer typePointer -> nodeId
getNodeIdFromPointer aPointer =
    case aPointer of
        FullNode nodeId ->
            nodeId

        TermAndType nodeId ->
            nodeId

        ContPointer nodeId _ ->
            nodeId

        TermPointer nodeId _ ->
            nodeId

        TypePointer nodeId _ ->
            nodeId



-- USER INPUT


type MenuState
    = SelectRule
    | VarRule
    | AppRule
    | AbsRule
    | CreateStartingNode


changeState : MenuState -> Model -> Model
changeState menuState model =
    { model
        | menuState = menuState
        , displayMessage =
            if List.member menuState [ VarRule, AbsRule, AppRule ] then
                "Fill out the input fields and hit 'Apply'!"

            else if menuState == SelectRule then
                "Select the corresponding inference rule!"

            else
                ""
    }


{-| If there is a greek counterpart for the given `char` we transform it to its greek representation we use for typing, otherwise `char` remains unchanged.
-}
charLatinToGreekRepresentation : Char -> Char
charLatinToGreekRepresentation char =
    case char of
        'a' ->
            'α'

        'b' ->
            'β'

        'd' ->
            'δ'

        'e' ->
            'ε'

        'f' ->
            'φ'

        'g' ->
            'γ'

        'h' ->
            'η'

        'i' ->
            'ι'

        'k' ->
            'κ'

        'l' ->
            'λ'

        'm' ->
            'μ'

        'n' ->
            'ν'

        'o' ->
            'ο'

        'p' ->
            'π'

        'r' ->
            'ρ'

        's' ->
            'σ'

        't' ->
            'τ'

        'u' ->
            'υ'

        'x' ->
            'χ'

        'z' ->
            'ζ'

        _ ->
            char
