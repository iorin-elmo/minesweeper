module Type exposing (..)
import Random
import Dict exposing (Dict)
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)

type Tile
    = Safe
    | Bomb
    | Flag
    | Open

type GameStatus
    = Loading
    | Playing
    | GameOver

type Msg
    = Opened (Int, Int)
    | Flagged (Int, Int)
    | Hovered (Int, Int)
    | HoverOut
    | TestStart
    | GetSeed Random.Seed
    | NoOp

type alias Model =
    { field : Field  -- (Int,Int) means coordinates
    , gameStatus : GameStatus       -- game status ... loading/playing/gameover
    , flags : Int                   -- count of flags
    , bombs : Int                   -- count of bombs
    , hover : (Int, Int)
    , seed : Int
    , bombOrNot : BombOrNot
    , navKey : Nav.Key
    , url : Url
    , size : Int
    }

type alias BombOrNot = ( List (Int, Int), List (Int, Int) )
type alias Field = Dict (Int, Int) Tile

--listMap : (a -> b) -> BombOrNot -> b