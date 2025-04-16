module Game exposing (..)
import Type exposing (..)
import Dict exposing (Dict)
import Field exposing (..)
import Random


genRandomInt = Random.int 0 Random.maxInt

gameUpdate : Msg -> Model -> Model
gameUpdate msg model =
    case msg of
        TestStart ->
            let
                (newBombOrNot, newField) =
                    fieldGenerator model.size model.seed model.bombOrNot

                newField_ =
                    randomOpen model.size model.seed newBombOrNot newField

                bombs = newBombOrNot |> Tuple.first |> List.length

            in
            { model
            | gameStatus = Playing
            , bombOrNot = newBombOrNot
            , field = newField_
            , bombs = bombs
            }


        Opened (x,y) ->
            case Dict.get (x,y) model.field of
                Just Safe -> { model | field = Dict.insert (x,y) Open model.field }
                Just Bomb -> { model | gameStatus = GameOver, hover = (-100, -100) }
                Just Open ->
                    let
                        newField = openSurround (x,y) model.size model.field |> flagSurround (x,y) model.size
                        newFlagCount = allFlagCount newField
                    in
                        { model
                        | field = newField
                        , flags = newFlagCount
                        , gameStatus =
                            if newFlagCount == model.bombs
                            then GameOver
                            else model.gameStatus
                        , hover =
                            if newFlagCount == model.bombs
                            then (-100, -100)
                            else model.hover
                        }

                _ -> model

        Flagged (x,y) ->
            case Dict.get (x,y) model.field of
                Just Safe -> { model | gameStatus = GameOver, hover = (-100, -100) }
                Just Bomb ->
                    if model.flags == model.bombs - 1
                    then
                        { model
                        | field = Dict.insert (x,y) Flag model.field
                        , flags = model.flags + 1
                        , gameStatus = GameOver
                        , hover = (-100, -100)
                        }
                    else
                        { model
                        | field = Dict.insert (x,y) Flag model.field
                        , flags = model.flags + 1
                        }
                _ -> model

        Hovered c ->
            { model | hover = c }

        HoverOut ->
            { model | hover = (-100,-100) }

        _ -> model



---- UTILITIES

openSurround (x,y) s dic =
    let
        isAllFlagged =
            Dict.filter (\c tile -> List.any ((==) c) (adjacency (x,y) s) && tile == Bomb) dic |> Dict.isEmpty
    in
        if isAllFlagged
        then
            Dict.map
                (\c tile ->
                    if List.any ((==) c) (adjacency (x,y) s) && tile == Safe
                    then Open
                    else tile
                ) dic

        else
            dic

flagSurround (x,y) s dic =
    let
        isAllOpened = Dict.filter (\c tile -> List.any ((==) c) (adjacency (x,y) s) && tile == Safe) dic |> Dict.isEmpty
    in
        if isAllOpened
        then
            Dict.map
                (\c tile ->
                    if List.any ((==) c) (adjacency (x,y) s) && tile == Bomb
                    then Flag
                    else tile
                ) dic

        else
            dic