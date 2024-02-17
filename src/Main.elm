module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import ListSelection
import Msg exposing (Msg(..))
import ShoppingList exposing (ShoppingList)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Screen
    = ListSelection
    | ListCreator String
    | ShoppingList String
    | ItemCreator String String


type alias Model =
    { screen : Screen
    , shoppingLists : Dict String ShoppingList.ShoppingList
    }


initialModel =
    { screen = ListSelection
    , shoppingLists = shoppingLists
    }


mapShoppingList : String -> (ShoppingList -> ShoppingList) -> Model -> Model
mapShoppingList listName mapper model =
    { model
        | shoppingLists = Dict.update listName (Maybe.map mapper) model.shoppingLists
    }


mapCurrentShoppingList : (ShoppingList -> ShoppingList) -> Model -> Model
mapCurrentShoppingList mapper model =
    case model.screen of
        ShoppingList list ->
            mapShoppingList list mapper model

        _ ->
            model


completeItem : String -> ShoppingList -> ShoppingList
completeItem item list =
    { list
        | pending = List.filter (\i -> i /= item) list.pending
        , completed = item :: list.completed
    }


addItem : String -> ShoppingList -> ShoppingList
addItem item list =
    { list
        | completed = List.filter (\i -> i /= item) list.completed
        , pending = item :: list.pending
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectList l ->
            { model | screen = ShoppingList l }

        BackToListSelection ->
            { model | screen = ListSelection }

        CompleteItem item ->
            mapCurrentShoppingList (completeItem item) model

        AddItem item ->
            mapCurrentShoppingList (addItem item) model

        OpenItemCreator ->
            Debug.todo "OpenItemCreator"

        OpenListCreator ->
            Debug.todo "OpenListCreator"


view : Model -> Html Msg
view model =
    let
        listSelectionScreen =
            ListSelection.listSelectionPageView (Dict.values model.shoppingLists)

        displayShoppingListWith : (ShoppingList -> Html Msg) -> String -> Html Msg
        displayShoppingListWith shoppingListView shoppingListName =
            Dict.get shoppingListName model.shoppingLists
                |> Maybe.map shoppingListView
                |> Maybe.withDefault listSelectionScreen
    in
    case model.screen of
        ListSelection ->
            listSelectionScreen

        ListCreator l ->
            listSelectionScreen

        ShoppingList l ->
            displayShoppingListWith ShoppingList.shoppingListPageView l

        ItemCreator l item ->
            displayShoppingListWith ShoppingList.shoppingListPageView l



-- ShoppingList.shoppingListView marketShoppingList


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
