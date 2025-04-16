module View exposing (..)
import Type exposing (..)
import Browser
import Html
import Html.Attributes
import Html.Events as HEv
import Svg exposing (svg)
import Svg.Attributes as At
import Svg.Events as Ev
import Dict exposing (Dict)
import JsonEvent exposing (..)
import Field exposing (..)

tileSize = 70
viewBox s =
    "0 0 " ++
    String.fromInt (s * tileSize + 15) ++ " " ++
    String.fromInt (s * tileSize + 15)

view : Model -> Browser.Document Msg
view model =
    { title = "MineSweeper"
    , body =
        [ Html.div
            []
            [ Html.div []
                [ gameStatusView model ]
            , svg
                [ At.width <| String.fromInt <| model.size * tileSize
                , At.height <| String.fromInt <| model.size * tileSize
                , At.viewBox <| viewBox model.size
                ]
                <| case model.gameStatus of
                        Loading -> []
                        _ -> fieldView model
            ]
        ]
    }


gameStatusView model =
    case model.gameStatus of
        Loading -> Html.text "Loading..."
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
            [ Svg.rect (
                    [ At.x <| String.fromInt <| x*tileSize+5
                    , At.y <| String.fromInt <| y*tileSize+5
                    , At.width <| String.fromInt <| tileSize+5
                    , At.height <| String.fromInt <| tileSize+5

                    , At.stroke "white"
                    , At.strokeWidth "4"
                    , At.fill <| if isInsideHoverSight then "#292929" else "black"
                    ]
                    ++
                    if model.gameStatus == Playing
                    then
                        [ Ev.onClick <| Opened (x,y)
                        , rightClickEvent <| Flagged (x,y)
                        , Ev.onMouseOver <| Hovered (x,y)
                        , Ev.onMouseOut <| HoverOut
                        ]
                    else []
                    )
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