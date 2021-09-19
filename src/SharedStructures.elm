module SharedStructures exposing (..)

import Dict exposing (Dict)


{-| The type alias for `Model` which represents the complete program state of this application.
-}
type alias Model =
    { viewState : ViewState
    , ruleTree : RuleTree
    , selectedNodeID : List Int
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


{-| The type `Msg` which is responsible for communicating
user interaction to the `update` function.
-}
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
    | ChangeState ViewState
    | KeyDown String
    | UrlChanged String
    | StartClick
    | GetUrlClick
    | ToggleLatinView
    | OpenStartPage
    | OpenHelpText
    | NoOperation



-- STLC


{-| The type alias for `TermVar` which is used for term variables.
-}
type alias TermVar =
    String


{-| The type `Type basicType` which is used for types in the lambda calculi.
The type variable `basicType` defines the type for _type variables_ .
-}
type AType basicType
    = BasicType basicType
    | Arrow (AType basicType) (AType basicType)
    | Unknown


{-| The type alias `SType` which is used for types of the `STLC`.
-}
type alias SType =
    AType String


{-| An abstract type for _contexts_, i.e. a `Dict` for typing assumptions.
-}
type AContext termVar typ
    = Context (Dict termVar typ)


{-| The type alias for the context used for the STLC.
-}
type alias SContext =
    AContext TermVar SType


{-| The type of terms of the lambda calculi.
-}
type Term
    = Var TermVar
    | Abs TermVar Term
    | App Term Term


{-| A `RuleTree` used for type inference in the STLC
in natural deduction style.
-}
type RuleTree
    = RVar SContext Term SType Bool
    | RAbs SContext Term SType RuleTree
    | RApp SContext Term SType RuleTree RuleTree
    | Hole


{-| Defines a set of input kinds from the user interface.
-}
type InputKind
    = GammaInput
    | XInput
    | MInput
    | NInput
    | SigmaInput
    | TauInput
    | RuleSelection


{-| A pointer which is used for indexing sub parts of typing judgements,
e.g. to accurately highlight conflicts in the process of type inference.
-}
type APointer nodeID contPointer termPointer typePointer
    = FullNode nodeID
    | TermAndType nodeID
    | ContPointer nodeID contPointer
    | TermPointer nodeID termPointer
    | TypePointer nodeID typePointer


{-| A helper function for generically transforming an abstract pointer `APointer`
through given functions.
-}
fmapPointer :
    (nodeID -> newNodeID)
    -> (contPointer -> newContPointer)
    -> (termPointer -> newTermPointer)
    -> (typePointer -> newTypePointer)
    -> APointer nodeID contPointer termPointer typePointer
    -> APointer newNodeID newContPointer newTermPointer newTypePointer
fmapPointer nodeIDFunc contPointerFunc termPointerFunc typePointerFunc aPointer =
    case aPointer of
        FullNode nodeID ->
            FullNode (nodeIDFunc nodeID)

        TermAndType nodeID ->
            TermAndType (nodeIDFunc nodeID)

        ContPointer nodeID contPointer ->
            ContPointer (nodeIDFunc nodeID) (contPointerFunc contPointer)

        TermPointer nodeID termPointer ->
            TermPointer (nodeIDFunc nodeID) (termPointerFunc termPointer)

        TypePointer nodeID typePointer ->
            TypePointer (nodeIDFunc nodeID) (typePointerFunc typePointer)


{-| For given list of `APointer` the `nodeID` will be discarded and
replaced by `()`.
-}
discardNodeIDs : List (APointer nodeID contPointer termPointer typePointer) -> List (APointer () contPointer termPointer typePointer)
discardNodeIDs =
    List.map <| fmapPointer (\_ -> ()) identity identity identity


type alias Pointer =
    APointer (List Int) ContPointer TermPointer TypePointer


type AContPointer var
    = FullContext
    | FullAssump var
    | JustVarFromAssump var
    | JustTypFromAssump var


type alias AContextHandler t var typ =
    { t
        | showTermVar : var -> String
        , showTypeForView : typ -> Bool -> String
    }


type alias SContextHandler t =
    AContextHandler t TermVar SType


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


getNodeIDFromPointer : APointer nodeID contPointer termPointer typePointer -> nodeID
getNodeIDFromPointer aPointer =
    case aPointer of
        FullNode nodeID ->
            nodeID

        TermAndType nodeID ->
            nodeID

        ContPointer nodeID _ ->
            nodeID

        TermPointer nodeID _ ->
            nodeID

        TypePointer nodeID _ ->
            nodeID



-- USER INPUT


type ViewState
    = Start
    | Help
    | SelectRule
    | VarRule
    | AppRule
    | AbsRule


changeViewState : ViewState -> Model -> Model
changeViewState viewState model =
    { model
        | viewState = viewState
        , displayMessage =
            if List.member viewState [ VarRule, AbsRule, AppRule ] then
                "Fill out the input fields and hit 'Apply'!"

            else if viewState == SelectRule then
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
