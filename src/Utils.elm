module Utils exposing (contextsAreEqual, getSuccessEmoji, getTypeFromContext, varIsMissingInContext, variableAndTypeConflictExistingTypingAssumption)

import Array
import Dict
import SharedStructures exposing (AContext(..), SContext, SType, TermVar)


{-| Checks for given contexts if they are equal, i.e. all key-value-pairs appear in both contexts.
-}
contextsAreEqual : SContext -> SContext -> Bool
contextsAreEqual (Context dict1) (Context dict2) =
    Dict.toList dict1 == Dict.toList dict2


{-| Returns the type for given `var` in given `SContext`.
-}
getTypeFromContext : TermVar -> SContext -> Maybe SType
getTypeFromContext var (Context dict) =
    Dict.get var dict


{-| Returns `True` iff given `var` is not part of given `Context`.
-}
varIsMissingInContext : TermVar -> SContext -> Bool
varIsMissingInContext var (Context dict) =
    Dict.foldl
        (\varFromContext _ typingAssumptionIsMissing ->
            if typingAssumptionIsMissing then
                var /= varFromContext

            else
                False
        )
        True
        dict


{-| Checks variable typings judgements if the variable and
its type conflict with an existing typing assumption in the context.
Calling this function on the following typing judgements would return:

        x:α, y:β ⊢ x:γ => True
        x:α, y:β ⊢ x:α => False
        y:β ⊢ x:γ => False

As the last example shows the function always returns `False` if the according
typing assumption is missing. If you need to check for that, use `Utils.varIsMissingInContext`.

-}
variableAndTypeConflictExistingTypingAssumption : TermVar -> SType -> SContext -> Bool
variableAndTypeConflictExistingTypingAssumption var typ (Context dict) =
    Dict.get var dict
        |> Maybe.map (\typFromContext -> typFromContext /= typ)
        |> Maybe.withDefault False


{-| Returns one of three success emojis (🎈, 🎉, 🥳, 🏆) based on some given `rngInt` to emulate some kind of randomness.

For `rngInt` use something like `List.length` or `String.length` on some value that varies a bit.
Elms Random package requires to use Cmd, so for sake of simplicity we don't use that on this little casual function.

-}
getSuccessEmoji : Int -> String
getSuccessEmoji rngInt =
    let
        rngIntFix =
            if rngInt == 0 then
                1

            else
                rngInt
    in
    Array.fromList [ "🎈", "🎉", "🥳", "🏆" ] |> Array.get (modBy 4 rngIntFix) |> Maybe.withDefault "🥳"
