module Direction exposing (Direction(..), asList, mirror, rotateLeftwise, rotateRightwise, fromCoord, toAngle, toCoord)

{-|

@docs Direction, asList, mirror, rotateLeftwise, rotateRightwise, fromCoord, toAngle, toCoord

-}


{-| An abstract concept of a direction on a grid.
-}
type Direction
    = Up
    | Down
    | Left
    | Right


{-| Create a list of all directions
-}
asList : ( Direction, List Direction )
asList =
    ( Up, [ Down, Left, Right ] )


{-| Rotates a `Direction` for 180 Degrees.

    Up
    |> mirror
    --> Down

    Left
    |> mirror
    --> Right

-}
mirror : Direction -> Direction
mirror direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


{-| Rotates a `Direction` clockwise

    Up
        |> rotateLeftwise
        --> Left

-}
rotateLeftwise : Direction -> Direction
rotateLeftwise direction =
    case direction of
        Up ->
            Left

        Left ->
            Down

        Down ->
            Right

        Right ->
            Up


{-| Rotates a `Direction` counter-clockwise

    Up
        |> rotateRightwise
        --> Right

-}
rotateRightwise : Direction -> Direction
rotateRightwise direction =
    case direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


{-| Convert coordinates into a direction by comparing the sign

    { x = 0, y = 1 }
        |> fromCoord
        --> Just Down

    { x = 0, y = -1 }
        |> fromCoord
        --> Just Up

    { x = 1, y = 0 }
        |> fromCoord
        --> Just Right

    { x = -1, y = 0 }
        |> fromCoord
        --> Just Left

    { x = 1, y = 1 }
        |> fromCoord
        --> Nothing

-}
fromCoord : { x : Int, y : Int } -> Maybe Direction
fromCoord pos =
    case ( pos.x, pos.y ) of
        ( 0, y ) ->
            if y > 0 then
                Just Down

            else
                Just Up

        ( x, 0 ) ->
            if x > 0 then
                Just Right

            else
                Just Left

        _ ->
            Nothing


{-| Convert a Direction into a coord.

    let
        list = asList
            |> (\(head,tail) -> head :: tail)
    in
        list
        |> List.map toCoord
        |> List.map fromCoord
        --> list

    Right
        |> toCoord
        --> {x = 1, y = 0}

-}
toCoord : Direction -> { x : Int, y : Int }
toCoord dir =
    case dir of
        Right ->
            { x = 1, y = 0 }

        Down ->
            { x = 0, y = 1 }

        Left ->
            { x = -1, y = 0 }

        Up ->
            { x = 0, y = -1 }


{-| Convert a direction into an angle.

    Right
    |> toAngle
    --> 0

    Up
    |> toAngle
    --> pi / 2

    Left
    |> toAngle
    --> pi

    Down
    |> toAngle
    --> 3 * pi / 2

-}
toAngle : Direction -> Float
toAngle dir =
    case dir of
        Right ->
            0

        Up ->
            pi / 2

        Left ->
            pi

        Down ->
            3 * pi / 2
