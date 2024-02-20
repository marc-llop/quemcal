module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import ItemCreation
import ListSelection
import ModelTypes exposing (Item, ShoppingList, ShoppingListName, shoppingListNameFromString, shoppingListNameToString)
import Msg exposing (Msg(..))
import ShoppingList
import SimpleTextIndex exposing (Index)
import String.Normalize


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


type Screen
    = ListSelection
    | ListCreation ShoppingListName
    | ShoppingList ShoppingListName
    | ItemCreation ShoppingListName Item


type alias Model =
    { screen : Screen
    , shoppingLists : Dict String ShoppingList
    , itemIndex : Index Item
    }


shoppingListsToItems : List ShoppingList -> List Item
shoppingListsToItems lists =
    List.concatMap
        (\{ completed, pending } -> completed ++ pending)
        lists


populateIndex : Dict a ShoppingList -> Index Item -> Index Item
populateIndex dict index =
    shoppingListsToItems (Dict.values dict)
        |> List.foldl SimpleTextIndex.add index


itemToString : Item -> String
itemToString a =
    a


initialModel =
    { screen = ListSelection
    , shoppingLists = shoppingLists
    , itemIndex =
        SimpleTextIndex.config
            { ref = itemToString
            , fields = [ itemToString ]
            , normalize = String.toLower >> String.Normalize.removeDiacritics
            }
            |> SimpleTextIndex.new
            |> populateIndex shoppingLists
    }


mapShoppingList : ShoppingListName -> (ShoppingList -> ShoppingList) -> Model -> Model
mapShoppingList listName mapper model =
    { model
        | shoppingLists = Dict.update (shoppingListNameToString listName) (Maybe.map mapper) model.shoppingLists
    }


mapCurrentShoppingList : (ShoppingList -> ShoppingList) -> Model -> Model
mapCurrentShoppingList mapper model =
    case model.screen of
        ShoppingList list ->
            mapShoppingList list mapper model

        ItemCreation list _ ->
            mapShoppingList list mapper model

        _ ->
            model


mapItemIndex : (Index Item -> Index Item) -> Model -> Model
mapItemIndex mapper model =
    { model | itemIndex = mapper model.itemIndex }


listWithout : Item -> List Item -> List Item
listWithout item =
    List.filter (\i -> i /= item)


completeItem : String -> ShoppingList -> ShoppingList
completeItem item list =
    { list
        | pending = listWithout item list.pending
        , completed = item :: list.completed
    }


addItem : String -> ShoppingList -> ShoppingList
addItem item list =
    { list
        | completed = listWithout item list.completed
        , pending = item :: listWithout item list.pending
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
                |> mapItemIndex (SimpleTextIndex.add item)

        OpenItemCreator ->
            case model.screen of
                ShoppingList list ->
                    { model | screen = ItemCreation list "" }

                _ ->
                    model

        OpenListCreator ->
            Debug.todo "OpenListCreator"

        UpdateEditedItem updatedItem ->
            case model.screen of
                ItemCreation list _ ->
                    { model | screen = ItemCreation list updatedItem }

                _ ->
                    model


view : Model -> Html Msg
view model =
    let
        listSelectionScreen =
            ListSelection.listSelectionPageView (Dict.values model.shoppingLists)

        displayShoppingListWith : (ShoppingList -> Html Msg) -> ShoppingListName -> Html Msg
        displayShoppingListWith shoppingListView shoppingListName =
            Dict.get (shoppingListNameToString shoppingListName) model.shoppingLists
                |> Maybe.map shoppingListView
                |> Maybe.withDefault listSelectionScreen
    in
    case model.screen of
        ListSelection ->
            listSelectionScreen

        ListCreation l ->
            listSelectionScreen

        ShoppingList l ->
            displayShoppingListWith ShoppingList.shoppingListPageView l

        ItemCreation l item ->
            ItemCreation.itemCreationPageView model.itemIndex l item


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
        |> List.map
            (\sl ->
                ( sl.name
                , { name = shoppingListNameFromString sl.name
                  , completed = sl.completed
                  , pending = sl.pending
                  }
                )
            )
        |> Dict.fromList
