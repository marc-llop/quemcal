module Main exposing (main)

import Browser
import Html exposing (Html)
import Msg exposing (Msg(..))
import ShoppingList


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
    ShoppingList.shoppingListView shoppingList


shoppingList =
    { name = "List title"
    , pending =
        [ "Potatoes"
        , "Cucumbers"
        , "Bananas"
        , "Tomatoes"
        , "Onions"
        , "Carrots"
        , "Spinachs"
        ]
    , completed =
        [ "Cockles"
        , "Tuna"
        , "Salmon"
        , "Haddock"
        , "Cod"
        ]
    }
