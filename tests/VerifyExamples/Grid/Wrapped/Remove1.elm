module VerifyExamples.Grid.Wrapped.Remove1 exposing (..)

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
    Test.test "#remove: \n\n    grid |> insert (2,2) 42 |> get (2,2)\n    --> Just 42" <|
        \() ->
            Expect.equal
                (
                grid |> insert (2,2) 42 |> get (2,2)
                )
                (
                Just 42
                )