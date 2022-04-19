module Position exposing (add, addTo, coordTo, random, toPoint)

{-|

@docs Position, move, add, addTo, coordTo, random, toPoint

-}

import Direction exposing (Direction(..))
import Random exposing (Generator)


{-| Generate a position inside a grid.
-}
random : { columns : Int, rows : Int } -> Generator ( Int, Int )
random { rows, columns } =
    Random.pair (Random.int 0 (rows - 1)) (Random.int 0 (columns - 1))


{-| Create the coordinates to the new location using the current position as the center.

    (1,1)
    |> coordTo (1,2)
    --> { x = 0, y = 1 }

-}
coordTo : ( Int, Int ) -> ( Int, Int ) -> { x : Int, y : Int }
coordTo ( x1, y1 ) ( x2, y2 ) =
    { x = x1 - x2, y = y1 - y2 }


{-| Add coordinates to a position

    (2,3)
    |> add { x = 1, y = 0}
    --> (3,3)

-}
add : { x : Int, y : Int } -> ( Int, Int ) -> ( Int, Int )
add { x, y } ( x2, y2 ) =
    ( x2 + x, y2 + y )


{-| Add coordinates to a position

    { x = 1, y = 0 }
    |> addTo (2,3)
    --> (3,3)

-}
addTo : ( Int, Int ) -> { x : Int, y : Int } -> ( Int, Int )
addTo ( x2, y2 ) { x, y } =
    ( x2 + x, y2 + y )


{-| given the width and height in in Int, returns a point with values in (-1,1)
-}
toPoint : { columns : Int, rows : Int } -> ( Int, Int ) -> ( Float, Float )
toPoint { rows, columns } ( x, y ) =
    ( -1 + 2 * toFloat x / toFloat (columns - 1)
    , -1 + 2 * toFloat y / toFloat (rows - 1)
    )
