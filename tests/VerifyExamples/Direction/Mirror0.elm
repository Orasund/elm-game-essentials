module VerifyExamples.Direction.Mirror0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Direction exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#mirror: \n\n    Left\n    |> mirror\n    --> Right" <|
        \() ->
            Expect.equal
                (
                Left
                |> mirror
                )
                (
                Right
                )