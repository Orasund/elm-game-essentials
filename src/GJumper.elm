module GJumper exposing
    ( GameData, Status(..)
    , define, InitModel, View, view
    , withGui, footer, header, Footer, Header, Game
    )

{-| The GJumper virtual console is a template for grid based games.
It has the following Properties:

  - It displayes a 16x16 screen around the player. The player is always positioned
    at (7,7) within the screen.
  - It provides a 1x16 high header and a 3x16 high footer.
  - It uses the Buttons WASD (or arrow keys) for movement and ESC to reset the game.
  - Pressing a WASD button will first move the player and THEN trigger the tick-function.

Here is a list of games that use this virtual console:

  - [One Switch](https://orasund.itch.io/one-switch)
  - [Swappernaut](https://orasund.itch.io/swappernaut)

You can find the source files in the examples folder.


# Main Type

@docs GameData, Status


# Defining a Game

@docs define, InitModel, View, view


# Additional Features

@docs withGui, footer, header, Footer, Header, Game

-}

import GJumper.Core as Core
import Grid exposing (Grid)
import Grid.Direction exposing (Direction(..))
import Grid.Position as Position exposing (Position)
import PixelEngine exposing (Area, Background, Input(..), PixelEngine)
import PixelEngine.Image exposing (Image)
import PixelEngine.Options as Options exposing (Options)
import PixelEngine.Tile exposing (Tile, Tileset)
import Random exposing (Generator, Seed)


{-| The `Model` of a GJumper Game contains a player position, a grid and some
aditional data.

You can define a `square` how ever you want. Any information, that can not be
stored in the grid should be part of the `data`.

-}
type alias GameData square data =
    { data : data
    , grid : Grid square
    , player : Position
    }


{-| At any point in time a game is either `Ongoing`, `Won` or `Lost`. Once the game is
done, the game can be reset by pressing a buttion, some of the data can be also
transfered from one game session to another (like a level counter).
-}
type Status
    = Ongoing
    | Won
    | Lost


{-| The main function of the game console.

  - init : The intial model (dependent on randomnes). It maybe inputs data
    from a previous run (like a level counter).
  - isSolid : Specifies whether a square is solid
  - tick : Updates the game (dependent on randomnes) and also returns the current
    status of the game. This function will only be called if the player could
    successfully move (if the square is not solid)
  - view : Specifies the gui.
  - title : The title of the game
  - imgSize : The size of a square, tile.
  - gameWon : A end screen after the game has been won. The screen is
    imgSize\*16 x imgSize\*20 big.
  - gameOver : A end screen after the game has been lost. The screen is
    imgSize\*16 x imgSize\*20 big.

-}
define :
    { init : Maybe data -> Generator (InitModel square data)
    , isSolid : square -> Bool
    , tick : GameData square data -> Generator ( GameData square data, Status )
    , view : data -> View square
    , title : String
    , imgSize : Int
    , gameWon : List ( ( Float, Float ), Image Never )
    , gameOver : List ( ( Float, Float ), Image Never )
    }
    -> Game square data
define config =
    PixelEngine.game
        { init = init
        , update =
            update
                { initfun = config.init
                , isSolid = config.isSolid
                , tick = config.tick
                }
        , subscriptions = always Sub.none
        , view =
            viewFun
                config.view
                config.title
                { gameWon = config.gameWon, gameOver = config.gameOver }
                config.imgSize
        , controls = controls
        , width = toFloat config.imgSize * 16
        }


{-| The initial model.

  - data : Some custom data (anything you like)
  - player : The starting position of the player on the grid
  - rows : The height of the grid
  - columns : The width of the grid
  - level : A Matrix (list of lists) of squares. If an entry is Nothing, it might
    be filled later with additional stuff.
  - fixed : After the level has be created a fixed amount of squares will be
    randomly placed within all empty squares.
  - distribution: Next all empty sqaures get filled regarding the given distribution.
    The Float stated the relative occurence of the given spare. If some spots should
    stay empty, you need to add `(float,Nothing)` to the list where `float` is higher then 0.

-}
type alias InitModel square data =
    { data : data
    , player : Position
    , distribution :
        data
        -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
    , fixed : data -> List ( Int, square )
    , level : data -> List (List (Maybe square))
    , rows : Int
    , columns : Int
    }


{-| The view of the model
-}
type alias View square =
    Core.View square


{-| Specifies how things look.

It uses the [Tile](https://package.elm-lang.org/packages/Orasund/pixelengine/latest/PixelEngine-Tile) from Orasund/pixelengine.

-}
view : { player : Tile Never, square : square -> Tile Never } -> Tileset -> Background -> View square
view { player, square } tileset background =
    Core.View
        { gui = ( Core.Header [], Core.Footer [] )
        , player = player
        , square = square
        , tileset = tileset
        , background =
            { grid = background
            , gui = background
            }
        }


{-| Adds a Gui to the view.
-}
withGui : Header -> Footer -> Background -> View square -> View square
withGui h f b (Core.View ({ background } as v)) =
    Core.View
        { v
            | gui = ( h, f )
            , background =
                { background
                    | gui = b
                }
        }


{-| A header
-}
type alias Header =
    Core.Header


{-| A Footer
-}
type alias Footer =
    Core.Footer


{-| A header, the floats specify the x position. It must be between 0 and 16\*imgSize.
-}
header : List ( Float, Image Never ) -> Header
header =
    Core.Header


{-| A footer. the floats specify the x position. It must be between 0 and 16\*imgSize.
The y position depends on the list (first list has y=0, second list has y=1 and third has y = 2)
-}
footer : List ( Float, Image Never ) -> List ( Float, Image Never ) -> List ( Float, Image Never ) -> Footer
footer l1 l2 l3 =
    let
        mapList : Float -> List ( Float, Image Never ) -> List ( ( Float, Float ), Image Never )
        mapList y =
            List.map (\( x, img ) -> ( ( x, y ), img ))
    in
    [ l1 |> mapList 0
    , l2 |> mapList 1
    , l3 |> mapList 2
    ]
        |> List.concat
        |> Core.Footer


{-| The type of a game.
-}
type alias Game square model =
    PixelEngine () (Model square model) Msg



--------------------------------------------------------------------------------


type Model square data
    = Loading
    | Running
        { gameData : GameData square data
        , seed : Seed
        , status : Status
        }


type Msg
    = GotSeed Seed
    | Move Direction
    | Reset


init : () -> ( Model square model, Cmd Msg )
init _ =
    ( Loading
    , Random.generate GotSeed Random.independentSeed
    )


update :
    { initfun :
        Maybe data
        ->
            Generator
                { data : data
                , player : Position
                , distribution :
                    data
                    -> ( ( Float, Maybe square ), List ( Float, Maybe square ) )
                , fixed : data -> List ( Int, square )
                , level : data -> List (List (Maybe square))
                , rows : Int
                , columns : Int
                }
    , isSolid : square -> Bool
    , tick : GameData square data -> Generator ( GameData square data, Status )
    }
    -> Msg
    -> Model square data
    -> ( Model square data, Cmd Msg )
update { initfun, isSolid, tick } msg model =
    case ( msg, model ) of
        ( GotSeed seed, Loading ) ->
            seed
                |> Random.step
                    (initfun Nothing
                        |> Random.andThen
                            (\{ data, player, distribution, fixed, level, rows, columns } ->
                                Core.gridGenerator data
                                    { distribution = distribution
                                    , fixed = fixed
                                    , level = level
                                    , rows = rows
                                    , columns = columns
                                    }
                                    |> Random.map
                                        (\grid ->
                                            { grid = grid |> Grid.remove player
                                            , player = player
                                            , data = data
                                            }
                                        )
                            )
                    )
                |> (\( gameData, s ) ->
                        ( Running { seed = s, gameData = gameData, status = Ongoing }
                        , Cmd.none
                        )
                   )

        ( Move dir, Running { status, gameData, seed } ) ->
            let
                newPos : Position
                newPos =
                    gameData.player
                        |> Position.move 1 dir
                        |> (\( x, y ) -> ( x |> modBy 16, y |> modBy 16 ))
            in
            if status /= Ongoing then
                seed
                    |> Random.step
                        (initfun (Just gameData.data)
                            |> Random.andThen
                                (\{ data, player, distribution, fixed, level, rows, columns } ->
                                    Core.gridGenerator data
                                        { distribution = distribution
                                        , fixed = fixed
                                        , level = level
                                        , rows = rows
                                        , columns = columns
                                        }
                                        |> Random.map
                                            (\grid ->
                                                { grid = grid |> Grid.remove player
                                                , player = player
                                                , data = data
                                                }
                                            )
                                )
                        )
                    |> (\( gD, s ) ->
                            ( Running { seed = s, gameData = gD, status = Ongoing }
                            , Cmd.none
                            )
                       )

            else if
                gameData.grid
                    |> Grid.get newPos
                    |> Maybe.map isSolid
                    |> Maybe.withDefault False
            then
                ( model, Cmd.none )

            else
                seed
                    |> Random.step (tick { gameData | player = newPos })
                    |> (\( ( gd, st ), s ) ->
                            ( Running
                                { gameData = gd
                                , seed = s
                                , status = st
                                }
                            , Cmd.none
                            )
                       )

        ( Reset, _ ) ->
            init ()

        _ ->
            ( model, Cmd.none )


controls : Input -> Maybe Msg
controls input =
    case input of
        InputUp ->
            Just <| Move Up

        InputDown ->
            Just <| Move Down

        InputLeft ->
            Just <| Move Left

        InputRight ->
            Just <| Move Right

        InputB ->
            Just <| Reset

        _ ->
            Nothing


areas : (data -> View square) -> GameData square data -> Int -> List (Area msg)
areas fun gameData imgSize =
    let
        { data, player, grid } =
            gameData

        (Core.View render) =
            fun data

        ( h, f ) =
            render.gui

        ( playerX, playerY ) =
            player

        { columns, rows } =
            grid |> Grid.dimensions
    in
    grid
        |> Grid.toList
        |> List.filterMap
            (\( ( x, y ), a ) ->
                let
                    newX : Int
                    newX =
                        x - playerX + 7 |> modBy columns

                    newY : Int
                    newY =
                        y - playerY + 7 |> modBy rows
                in
                if newX < 16 && newY < 16 then
                    Just
                        ( ( newX
                          , newY
                          )
                        , a
                        )

                else
                    Nothing
            )
        |> List.map
            (\( pos, square ) ->
                ( pos, square |> render.square )
            )
        |> (::) ( ( 7, 7 ), render.player )
        |> Core.create render.background render.tileset
        |> Core.withHeader h
        |> Core.withFooter f
        |> Core.toAreas imgSize
        |> List.map (PixelEngine.mapArea never)


viewFun :
    (model -> View square)
    -> String
    ->
        { gameWon : List ( ( Float, Float ), Image Never )
        , gameOver : List ( ( Float, Float ), Image Never )
        }
    -> Int
    -> Model square model
    -> { title : String, options : Maybe (Options msg), body : List (Area msg) }
viewFun fun title { gameWon, gameOver } imgSize model =
    case model of
        Loading ->
            { title = title
            , options = Just Options.default
            , body = []
            }

        Running { gameData, status } ->
            let
                (Core.View render) =
                    fun gameData.data
            in
            case status of
                Ongoing ->
                    { title = title
                    , options = Just Options.default
                    , body = areas fun gameData imgSize
                    }

                Lost ->
                    { title = title
                    , options =
                        Options.default
                            |> Options.withTransitionFrom
                                (areas fun gameData imgSize)
                                (Options.transition
                                    "death_transition"
                                    { start = "opacity:1;filter:grayscale(10%) blur(0px);"
                                    , keyFrames =
                                        [ Just "opacity:1;filter:grayscale(70%) blur(0px);"
                                        , Nothing
                                        ]
                                    , end = "opacity:0;filter:grayscale(70%) blur(5px);"
                                    }
                                )
                            |> Just
                    , body =
                        gameOver
                            |> PixelEngine.imageArea
                                { background = render.background.gui
                                , height = toFloat <| imgSize * 20
                                }
                            |> PixelEngine.mapArea never
                            |> List.singleton
                    }

                Won ->
                    { title = title
                    , options =
                        Options.default
                            |> Options.withTransitionFrom
                                (areas fun gameData imgSize)
                                (Options.transition
                                    "win_transition"
                                    { start = "opacity:1;filter:brightness(100%) blur(0px);"
                                    , keyFrames =
                                        [ Just "opacity:1;filter:brightness(170%) blur(0px);"
                                        , Nothing
                                        ]
                                    , end = "opacity:0;filter:brightness(170%) blur(5px);"
                                    }
                                )
                            |> Just
                    , body =
                        gameWon
                            |> PixelEngine.imageArea
                                { background = render.background.gui
                                , height = toFloat <| imgSize * 20
                                }
                            |> PixelEngine.mapArea never
                            |> List.singleton
                    }
