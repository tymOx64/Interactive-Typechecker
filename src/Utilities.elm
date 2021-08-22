module Utilities exposing (getSuccessEmoji)

import Array


{-| Returns one of three success emojis (🎈, 🎉, 🥳) based on some given `rngInt` to emulate some kind of randomness.

For `rngInt` use something like `List.length` or `String.length` on some value that varies a bit.
Elms Random package requires to use Cmd, so for sake of simplicity we don't use that on this little casual function.

-}
getSuccessEmoji : Int -> String
getSuccessEmoji rngInt =
    Array.fromList [ "🎈", "🎉", "🥳" ] |> Array.get (modBy rngInt 3) |> Maybe.withDefault "🥳"
