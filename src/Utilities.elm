module Utilities exposing (getSuccessEmoji)

import Array


{-| Returns one of three success emojis (🎈, 🎉, 🥳) based on some given `rngInt` to emulate some kind of randomness.

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
    Array.fromList [ "🎈", "🎉", "🥳" ] |> Array.get (modBy rngIntFix 3) |> Maybe.withDefault "🥳"
