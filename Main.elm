module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes
import Html.Events as HEv
import Svg exposing (svg)
import Svg.Events as Ev
import Svg.Attributes as At
import Dict exposing (Dict)
import Json.Decode as D
import Random
import Random.List
import Url exposing (Url)

defaultSize = 5


type Tile
    = Safe
    | Bomb
    | Flag
    | Open

type GameStatus
    = Loading
    | Playing
    | GameOver

type alias Model =
    { field : Dict (Int, Int) Tile  -- (Int,Int) means coordinates
    , gameStatus : GameStatus       -- game status ... loading/playing/gameover
    , flags : Int                   -- count of flags
    , bombs : Int                   -- count of bombs
    , hover : (Int, Int)
    , seed : Int
    , bombOrNot : ( List (Int,Int), List (Int,Int) )
    , navKey : Nav.Key
    , url : Url
    , size : Int
    }


init : Nav.Key -> Url -> () -> (Model, Cmd Msg)
init navKey url _ =
    let
        newFieldSize =
            getQueryInt "size" (Maybe.withDefault "" url.query)
                |> Maybe.withDefault defaultSize

        initModel =
            { field =
                List.range 0 (newFieldSize-1)
                    |> List.concatMap
                        (\n ->
                            List.map (Tuple.pair n) (List.range 0 (newFieldSize-1))
                        )
                    |> List.map
                        (\c -> (c, Safe))
                    |> Dict.fromList
            , gameStatus = Loading
            , flags = 0
            , bombs = 10
            , hover = (-100,-100)
            , seed = seed
            , bombOrNot =
                ( List.range 0 (newFieldSize-1)
                    |> List.concatMap
                        (\n ->
                        List.map (Tuple.pair n) (List.range 0 (newFieldSize-1))
                    )
                , []
                )
            , navKey = navKey
            , url = url
            , size = newFieldSize
            }

        defaultReturn = (0, Random.generate GetSeed Random.independentSeed)

        (seed, cmd) =
            case url.query of
                Just queryString ->
                    queryString
                        |> getQueryInt "seed"
                        |> Maybe.map (\s -> Tuple.pair s Cmd.none)
                        |> Maybe.withDefault defaultReturn

                Nothing -> defaultReturn
    in
        (initModel, cmd)


type Msg
    = Opened (Int, Int)
    | Flagged (Int, Int)
    | Hovered (Int,Int)
    | HoverOut
    | TestStart
    | GetSeed Random.Seed
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetSeed seed ->
            let
                newSeed = Random.step genRandomInt seed |> Tuple.first
                newUrl = "?seed=" ++ String.fromInt newSeed ++ "&?size=" ++ String.fromInt model.size
            in
                ( { model | seed = newSeed }
                , Nav.pushUrl model.navKey newUrl
                )

        _ ->
            (gameUpdate msg model, Cmd.none)

gameUpdate : Msg -> Model -> Model
gameUpdate msg model =
    case msg of
        TestStart ->
            { model | gameStatus = Playing }
                |> fieldGenerator
                |> randomOpen

        Opened (x,y) ->
            case Dict.get (x,y) model.field of
                Just Safe -> { model | field = Dict.insert (x,y) Open model.field }
                Just Bomb -> { model | gameStatus = GameOver }
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
                        }

                _ -> model

        Flagged (x,y) ->
            case Dict.get (x,y) model.field of
                Just Safe -> { model | gameStatus = GameOver }
                Just Bomb ->
                    if model.flags == model.bombs - 1
                    then
                        { model
                        | field = Dict.insert (x,y) Flag model.field
                        , flags = model.flags + 1
                        , gameStatus = GameOver
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


tileSize = 70

view : Model -> Browser.Document Msg
view model =
    { title = "MineSweeper"
    , body =
        [ Html.div []
            [ Html.div [][ gameStatusView model ]
            , svg
                [ At.width <| String.fromInt <| model.size * tileSize
                , At.height <| String.fromInt <| model.size * tileSize
                , At.viewBox <| "0 0 1000 1000"
                ]
                <| case model.gameStatus of
                        Loading -> []
                        _ -> fieldView model
            ]
        ]
    }




main : Program () Model Msg
main =
    Browser.application
        { init = \flags url navKey -> init navKey url ()
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }

------- FIELD GENERATOR

fieldGenerator : Model -> Model
fieldGenerator model =
    let
        listGenerator =
            Random.List.shuffle (Tuple.first model.bombOrNot)
                |> Random.map
                    (\l ->
                        ( List.take model.bombs l
                        , List.drop model.bombs l
                        )
                    )

        (newBombOrNot, newSeed) =
            Random.step listGenerator (Random.initialSeed model.seed)
    in
        { model
        | bombOrNot = newBombOrNot
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
        (n, newSeed) =
            Random.step (Random.int 6 9) (Random.initialSeed model.seed)
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

gameStatusView model =
    case model.gameStatus of
        Loading -> Html.button [ HEv.onClick TestStart ][ Html.text "Start" ]
        Playing ->
            Html.text
                <| "Bombs remain : " ++ (String.fromInt <| model.bombs - model.flags) ++ " / " ++ (String.fromInt model.bombs)
        GameOver ->
            if model.flags == model.bombs
            then
                Html.text "Game Clear !!"
            else
                Html.text "Game Over !!"



fieldView model =
    List.range 0 (model.size-1)
        |> List.concatMap
                (\n ->
                    List.map (Tuple.pair n) (List.range 0 (model.size-1))
                )
            |> List.map (\c -> tileView c model)
            |> List.concat


tileView (x,y) model =
    let
        tile = Maybe.withDefault Safe <| Dict.get (x,y) model.field
        isInsideHoverSight =
            if  Dict.get model.hover model.field == Just Open
            then
                List.any ((==) (x,y)) <| model.hover :: adjacency model.hover model.size
            else
                model.hover == (x,y)
    in
            [ Svg.rect
                [ At.x <| String.fromInt <| x*tileSize+5
                , At.y <| String.fromInt <| y*tileSize+5
                , At.width <| String.fromInt <| tileSize+5
                , At.height <| String.fromInt <| tileSize+5
                , Ev.onClick <| Opened (x,y)
                , rightClickEvent <| Flagged (x,y)
                , At.stroke "white"
                , At.strokeWidth "4"
                , At.fill <| if isInsideHoverSight then "#292929" else "black"
                , Ev.onMouseOver <| Hovered (x,y)
                , Ev.onMouseOut <| HoverOut
                ]
                [
                ]
            , Svg.text_
                [ case tile of
                    Flag ->
                        At.x <| String.fromInt <| x*tileSize+5
                    _ ->
                        At.x <| String.fromInt <| x*tileSize+tileSize//4+5
                , At.y <| String.fromInt <| (y+1)*tileSize-tileSize//4+5
                , At.textAnchor "right"
                , At.fontSize <| String.fromInt <| tileSize//4*3
                , At.fill
                    <| if isTileCleared (x,y) model.size model.field then "#a6a6a6" else "white"
                , Html.Attributes.attribute "pointer-events" "none"
                ]
                [ case tile of
                    Open -> Svg.text <| String.fromInt <| bombCount (x,y) model.size model.field
                    Flag -> Svg.text "🚩"
                    _ -> Svg.text ""
                ]
            ]

rightClickEvent : Msg -> Svg.Attribute Msg
rightClickEvent msg =
    Ev.custom
        "contextmenu"
        ( D.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )

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


genRandomInt = Random.int 0 Random.maxInt

getQueryInt : String -> String -> Maybe Int
getQueryInt str query =
    query
    |> String.split "&"
    |> List.filterMap
        (\part ->
            case String.split "=" part of
                [ s, val ] -> if String.contains str s then String.toInt val else Nothing
                _ -> Nothing
        )
    |> List.head