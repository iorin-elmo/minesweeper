module Solver exposing (
    solve
    )

import Type exposing (..)
import Dict exposing (Dict)

solve : Dict (Int,Int) Tile -> Dict (Int,Int) Tile
solve field = field