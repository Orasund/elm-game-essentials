module VerifyExamples.Grid.Wrapped.IsEmpty1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Grid.Wrapped exposing (..)



grid : Grid a
grid =
    empty dimensions
dimensions : { columns:Int , rows:Int }
dimensions =
    { columns=42
    , rows=3
    }



spec1 : Test.Test
spec1 =
    Test.test "#isEmpty: \n\n    grid |> isEmpty\n    --> True" <|
        \() ->
            Expect.equal
                (
                grid |> isEmpty
                )
                (
                True
                )