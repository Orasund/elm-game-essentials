module VerifyExamples.Direction.FromCoord2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Direction exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "#fromCoord: \n\n    { x = 1, y = 0 }\n        |> fromCoord\n    --> Just Right" <|
        \() ->
            Expect.equal
                (
                { x = 1, y = 0 }
                    |> fromCoord
                )
                (
                Just Right
                )