module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import ListSelection
import Msg exposing (Msg(..))
import ShoppingList


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Screen
    = ListSelection
    | ShoppingList String


type alias Model =
    { screen : Screen
    , shoppingLists : Dict String ShoppingList.ShoppingList
    }


initialModel =
    { screen = ListSelection
    , shoppingLists = shoppingLists
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectList l ->
            { model | screen = ShoppingList l }

        BackToListSelection ->
            { model | screen = ListSelection }


view : Model -> Html Msg
view model =
    -- ShoppingList.shoppingListView marketShoppingList
    ListSelection.listSelectionView (Dict.values model.shoppingLists)


marketShoppingList =
    { name = "Market"
    , pending =
        [ "Cockles"
        , "Squid"
        , "Salmon"
        , "Haddock"
        , "Cod"
        ]
    , completed =
        [ "Potatoes"
        , "Cucumbers"
        , "Bananas"
        , "Tomatoes"
        , "Onions"
        , "Carrots"
        , "Spinachs"
        ]
    }


shoppingLists =
    [ marketShoppingList
    , { name = "Groceries"
      , pending = [ "Cookies", "Bread", "Milk" ]
      , completed = [ "Pizza", "Frankfurts" ]
      }
    , { name = "Don't put preservatives in food, it's gross.", pending = [ "Tuna", "Olives", "Asparagus", "Pickled onions" ], completed = [] }
    , { name = "Completed list", pending = [], completed = [ "Style ListSelection view" ] }
    , { name = "Empty list", pending = [], completed = [] }
    , { name = "Half-done list", pending = [ "Make the app work" ], completed = [ "Make some screens" ] }
    ]
        |> List.map (\sl -> ( sl.name, sl ))
        |> Dict.fromList
