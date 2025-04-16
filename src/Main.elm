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
import Type exposing (..)
import Field exposing (..)
import JsonEvent exposing (..)
import View exposing (..)
import Game exposing (..)
import Task

import Debug exposing (log)

defaultSize = 5

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

init : Nav.Key -> Url -> () -> (Model, Cmd Msg)
init navKey url _ =
    let
        newFieldSize =
            getQueryInt "size" (Maybe.withDefault "" url.query)
                |> Maybe.withDefault defaultSize
                |> max 5
                |> min 10

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
            , gameStatus = if seed /= 0 then Loading else Playing
            , flags = 0
            , bombs = 15
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
                        |> Maybe.map (\s -> Tuple.pair s (Task.perform Start <| Task.succeed ()))
                        |> Maybe.withDefault defaultReturn

                Nothing -> defaultReturn
    in
        (initModel, cmd)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetSeed seed ->
            let
                newSeed = Random.step genRandomInt seed |> Tuple.first
                newUrl = "/minesweeper/index.html?seed=" ++ String.fromInt newSeed ++ "&size=" ++ String.fromInt model.size
            in
                ( { model | seed = newSeed, gameStatus = Playing }
                , Cmd.batch
                    [ Nav.pushUrl model.navKey newUrl
                    , Task.perform Start <| Task.succeed ()
                    ]
                )

        _ ->
            (gameUpdate msg model, Cmd.none)



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

