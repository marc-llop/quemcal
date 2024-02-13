module Main exposing (main)

import Browser
import Element exposing (Element, paragraph, rgb255, text)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Msg exposing (Msg(..))


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view model =
    Element.layout [ Background.color colors.black ]
        (paragraph [ Font.color colors.lime ] [ text "Hello world!" ])


colors =
    { black = rgb255 6 2 6
    , grey = rgb255 10 2 10
    , lime = rgb255 159 234 34
    , lightLime = rgb255 214 246 162
    }
