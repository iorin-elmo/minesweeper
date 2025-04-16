module Field exposing (..)
import Type exposing (..)
import Random
import Random.List
import Dict exposing (Dict)




------- FIELD GENERATOR

fieldGenerator : Model -> Model
fieldGenerator model =
    let
        bombMean = model.size * model.size // 2
        bombDeviation = model.size // 3

        bombCountGenerator =
            Random.int (bombMean - bombDeviation) (bombMean + bombDeviation)

        bombs =
            Random.step bombCountGenerator (Random.initialSeed model.seed)
                |> Tuple.first

        listGenerator =
            Random.List.shuffle (Tuple.first model.bombOrNot)
                |> Random.map
                    (\l ->
                        ( List.take bombs l
                        , List.drop bombs l
                        )
                    )

        (newBombOrNot, newSeed) =
            Random.step listGenerator (Random.initialSeed model.seed)
    in
        { model
        | bombOrNot = newBombOrNot
        , bombs = bombs
        , field =
            Dict.map
                (\c tile ->
                    if List.any ((==) c) <| Tuple.first newBombOrNot
                    then Bomb
                    else tile
                ) model.field
        }

randomOpen : Model -> Model
randomOpen model =
    let
        openMin = model.size + 1
        openMax = model.size * 2
        (n, newSeed) =
            Random.step (Random.int openMin openMax) (Random.initialSeed model.seed)
    in
        { model
        | field =
            Dict.map
                (\c tile ->
                    if List.any ((==) c) (List.take n <| Tuple.second model.bombOrNot)
                    then Open
                    else tile
                ) model.field
        }


------- UTILITIES

count : (a -> Bool) -> List a -> Int
count f list =
    List.filter f list |> List.length

adjacency : (Int, Int) -> Int -> List (Int, Int)
adjacency (x,y) s =
    [ (x-1, y-1), (x, y-1), (x+1, y-1)
    , (x-1, y  ),           (x+1, y  )
    , (x-1, y+1), (x, y+1), (x+1, y+1)
    ]
        |> List.filter (\(x_, y_) -> x_ >= 0 && y_ >= 0 && x_ < s && y_ < s)

bombCount : (Int, Int) -> Int -> Dict (Int,Int) Tile -> Int
bombCount c s dic =
    adjacency c s
        |> count
            (\(x,y) ->
                Dict.get (x,y) dic == Just Bomb || Dict.get (x,y) dic == Just Flag
            )

isTileCleared : (Int, Int) -> Int -> Dict (Int, Int) Tile -> Bool
isTileCleared c s dic =
    adjacency c s
        |> count
            (\c_ ->
                let
                    tile = Dict.get c_ dic |> Maybe.withDefault Safe
                in
                    tile == Bomb || tile == Safe
            )
        |> ((==) 0)

allFlagCount : Dict (Int,Int) Tile -> Int
allFlagCount dic =
    Dict.filter (\_ tile -> tile == Flag) dic |> Dict.size


