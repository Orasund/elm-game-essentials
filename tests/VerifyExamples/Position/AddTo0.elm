module VerifyExamples.Position.AddTo0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Position exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#addTo: \n\n    { x = 1, y = 0 }\n    |> addTo (2,3)\n    --> (3,3)" <|
        \() ->
            Expect.equal
                (
                { x = 1, y = 0 }
                |> addTo (2,3)
                )
                (
                (3,3)
                )