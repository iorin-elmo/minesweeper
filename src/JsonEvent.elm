module JsonEvent exposing (..)
import Type exposing (..)
import Svg
import Svg.Attributes
import Svg.Events as Ev
import Json.Decode as D

------- JSON EVENT


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