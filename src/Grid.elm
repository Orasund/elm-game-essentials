module Grid exposing
    ( Grid
    , init, empty, build, random, getMember, insert, update, remove
    , isEmpty, member, get, set, size, dimensions
    , positions, emptyPositions, values, toList, fromList
    , toDict, fromDict
    , map, filter, find
    )

{-| A `Grid` is a dictionary that has a size constraint.
Here is an example where such a grid is used:
[Snake Example](https://orasund.github.io/pixelengine/#Snake).


# Grids

@docs Grid


# Build

@docs init, empty, build, random, getMember, insert, update, remove


# Query

@docs isEmpty, member, get, set, size, dimensions


# List

@docs positions, emptyPositions, values, toList, fromList


# Dict

@docs toDict, fromDict


# Transform

@docs map, filter, find

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Random exposing (Generator)


{-| A grid with a fixes amount of columns and rows.

It will wrap the borders (apply ModBy), making every (Int,Int) valid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> get ( -1, 0 )
    --> grid |> get ( (dimensions |> .columns) - 1, 0 )

If instead you want to have hard border around your grid, use `Grid.Bordered` instead.

-}
type alias Grid a =
    Array (Array a)


{-| Create a grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid a
    grid =
        empty dimensions

    init (always <| Just ()) dimensions |> emptyPositions
    --> []

-}
init : (( Int, Int ) -> a) -> { rows : Int, columns : Int } -> Array (Array a)
init fun args =
    Array.initialize args.columns
        (\j ->
            Array.initialize args.rows
                (\i -> fun ( i, j ))
        )


{-| Create an empty grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    empty dimensions
    --> init (always Nothing ) dimensions

-}
empty : { rows : Int, columns : Int } -> Array (Array (Maybe a))
empty =
    init (always Nothing)


{-| Insert a value at a (Int,Int) in a grid. Replaces value when there is a collision.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> getMember (2,2)
    --> Just 42

-}
insert : ( Int, Int ) -> a -> Array (Array (Maybe a)) -> Array (Array (Maybe a))
insert pos elem =
    set pos (Just elem)


{-| Set the element in the grid
-}
set : ( Int, Int ) -> a -> Array (Array a) -> Array (Array a)
set ( i, j ) elem grid =
    grid
        |> Array.get j
        |> Maybe.map (\array -> grid |> Array.set j (Array.set i elem array))
        |> Maybe.withDefault grid


{-| Update the value of a grid for a specific (Int,Int) with a given function.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> update (2,2) (always <| Just 42)
    --> grid |> insert (2,2) 42

-}
update : ( Int, Int ) -> (Maybe a -> Maybe a) -> Array (Array (Maybe a)) -> Array (Array (Maybe a))
update pos fun grid =
    grid
        |> get pos
        |> Maybe.map
            (\elem ->
                elem
                    |> fun
                    |> Maybe.map (\newElem -> grid |> insert pos newElem)
                    |> Maybe.withDefault (grid |> remove pos)
            )
        |> Maybe.withDefault grid


{-| Remove a vlaue from a grid. If the (Int,Int) is empty, no changes are made.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> getMember (2,2) --> Just 42
    grid |> insert (2,2) 42 |> remove (2,2) |> getMember (2,2)
    --> Nothing

-}
remove : ( Int, Int ) -> Array (Array (Maybe a)) -> Array (Array (Maybe a))
remove pos grid =
    grid |> set pos Nothing


{-| Determine if a grid is empty.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> isEmpty --> True
    grid |> insert (2,2) 42 |> isEmpty --> False

-}
isEmpty : Array (Array (Maybe a)) -> Bool
isEmpty grid =
    grid
        |> Array.toList
        |> List.all (\array -> array |> Array.toList |> List.all ((==) Nothing))


{-| Determine if a (Int,Int) is empty.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> member (2,2)
    --> True

-}
member : ( Int, Int ) -> Array (Array (Maybe a)) -> Bool
member pos grid =
    getMember pos grid /= Nothing


{-| Get the value associated with a (Int,Int). If the (Int,Int) is empty, return Nothing.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> getMember (2,2)
    --> Just 42

-}
getMember : ( Int, Int ) -> Array (Array (Maybe a)) -> Maybe a
getMember pos grid =
    get pos grid
        |> Maybe.andThen identity


{-| Get the element in the grid
-}
get : ( Int, Int ) -> Array (Array a) -> Maybe a
get ( i, j ) grid =
    grid
        |> Array.get j
        |> Maybe.andThen (Array.get i)


{-| Determine the number of values in the grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> size
    --> 1

-}
size : Array (Array (Maybe a)) -> Int
size grid =
    grid
        |> Array.toList
        |> List.concatMap
            (\array ->
                array
                    |> Array.toList
                    |> List.filter ((/=) Nothing)
            )
        |> List.length


{-| Return the dimensions of the grid.

    dim : { columns:Int , rows:Int }
    dim =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dim

    grid |> dimensions
    --> dim

-}
dimensions : Grid a -> { columns : Int, rows : Int }
dimensions grid =
    { columns = grid |> Array.length
    , rows =
        grid
            |> Array.get 0
            |> Maybe.map Array.length
            |> Maybe.withDefault 0
    }


{-| Get all non empty positions in a grid, sorted from lowest to highest.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> positions
    --> [(2,2)]

-}
positions : Array (Array (Maybe a)) -> List ( Int, Int )
positions grid =
    grid
        |> toList
        |> List.filterMap (\( first, second ) -> second |> Maybe.map (\_ -> first))


{-| Get all of the values in a grid, in the order of their (Int,Int)s.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe a)
    grid =
        empty dimensions

    grid |> insert (2,2) 42 |> values
    --> [42]

-}
values : Array (Array (Maybe a)) -> List a
values grid =
    grid |> toList |> List.filterMap Tuple.second


{-| Get all empty positions in a grid, sorted from lowest to highest.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe Int)
    grid =
        init
            (always <| Just <| 42)
            dimensions

    grid |> remove (2,2) |> emptyPositions
    --> [(2,2)]

-}
emptyPositions : Array (Array (Maybe a)) -> List ( Int, Int )
emptyPositions grid =
    grid
        |> map
            (\_ maybeMark ->
                case maybeMark of
                    Just _ ->
                        Nothing

                    Nothing ->
                        Just ()
            )
        |> positions


{-| Convert a grid into an association list of (Int,Int)-value pairs,
sorted by the (Int,Int).

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=2
        , rows=2
        }

    grid : Grid (Maybe Int)
    grid =
        empty dimensions

    grid |> insert (1,1) 42 |> toList
    --> [( (0,0), Nothing ),((1,0), Nothing ),((0,1), Nothing ),( (1,1), Just 42 )]

-}
toList : Array (Array a) -> List ( ( Int, Int ), a )
toList grid =
    grid
        |> Array.indexedMap
            (\j array ->
                array
                    |> Array.indexedMap
                        (\i a -> ( ( i, j ), a ))
                    |> Array.toList
            )
        |> Array.toList
        |> List.concat


{-| Convert an association list into a grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe Int)
    grid =
        empty dimensions

    [((2,2),42),((2,1),20)] |> fromList dimensions
    --> grid |> insert (2,2) 42 |> insert (2,1) 20

-}
fromList : { rows : Int, columns : Int } -> List ( ( Int, Int ), a ) -> Array (Array (Maybe a))
fromList args list =
    let
        dict =
            list |> Dict.fromList
    in
    args
        |> init
            (\pos ->
                dict
                    |> Dict.get pos
            )


{-| Convert a grid into an associated dictionary

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe Int)
    grid =
        empty dimensions
            |> insert (2,2) 42

    grid |> toDict |> fromDict dimensions |> getMember (2,2)
    --> Just 42

-}
toDict : Array (Array (Maybe a)) -> Dict ( Int, Int ) a
toDict array =
    array
        |> toList
        |> List.filterMap
            (\( first, second ) ->
                second |> Maybe.map (\elem -> ( first, elem ))
            )
        |> Dict.fromList


{-| Convert an dictionary to a grid

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe Int)
    grid =
        empty dimensions

    grid |> toDict |> fromDict dimensions |> getMember (2,2)
    --> Nothing

-}
fromDict : { rows : Int, columns : Int } -> Dict ( Int, Int ) a -> Array (Array (Maybe a))
fromDict config =
    Dict.toList >> fromList config


{-| Apply a function to **all** positions in a grid.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    empty dimensions |> map (\_ _ -> Just 42)
    --> init (always <| Just 42) dimensions

-}
map : (( Int, Int ) -> a -> b) -> Array (Array a) -> Array (Array b)
map fun grid =
    grid
        |> Array.indexedMap
            (\j ->
                Array.indexedMap
                    (\i -> fun ( i, j ))
            )


{-| Keep only the values that pass the given test.

    dimensions : { columns:Int , rows:Int }
    dimensions =
        { columns=42
        , rows=3
        }

    grid : Grid (Maybe Int)
    grid =
        empty dimensions
            |> insert (2,4) 2
            |> insert (2,2) 42

    grid |> filter (\_ -> (==) 42) |> values
    --> [42]

-}
filter : (( Int, Int ) -> a -> Bool) -> Array (Array (Maybe a)) -> Array (Array (Maybe a))
filter fun grid =
    grid
        |> map
            (\pos maybe ->
                maybe
                    |> Maybe.andThen
                        (\elem ->
                            if fun pos elem then
                                Just elem

                            else
                                Nothing
                        )
            )


{-| Find the first value that passes a given test.
-}
find : (( Int, Int ) -> a -> Bool) -> Array (Array a) -> Maybe ( ( Int, Int ), a )
find fun grid =
    let
        recFind : List ( ( Int, Int ), a ) -> Maybe ( ( Int, Int ), a )
        recFind list =
            case list of
                (( pos, elem ) as head) :: tail ->
                    if fun pos elem then
                        Just head

                    else
                        recFind tail

                [] ->
                    Nothing
    in
    grid
        |> toList
        |> recFind


{-| Build a grid out of lists
-}
build : List (List a) -> Array (Array a)
build list =
    list
        |> Array.fromList
        |> Array.map Array.fromList


{-| Generate a random grid based on a cell generator
-}
random : { columns : Int, rows : Int } -> Generator a -> Generator (Array (Array a))
random args gen =
    gen
        |> Random.list args.rows
        |> Random.list args.columns
        |> Random.map build
