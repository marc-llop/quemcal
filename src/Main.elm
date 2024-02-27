module Main exposing (main)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html)
import ItemCreation exposing (searchBarId)
import ListCreation
import ListSelection
import ModelTypes exposing (Item, ShoppingList, ShoppingListName, newShoppingList, shoppingListNameFromString, shoppingListNameToString)
import Msg exposing (Msg(..))
import Platform.Cmd as Cmd
import ShoppingList
import SimpleTextIndex exposing (Index)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type alias Flags =
    ()


type Screen
    = ListSelection
    | ListCreation String
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


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { screen = ListSelection
      , shoppingLists = shoppingLists
      , itemIndex =
            SimpleTextIndex.config
                { ref = itemToString
                , fields = [ itemToString ]
                , normalize = ModelTypes.normalizeItem
                }
                |> SimpleTextIndex.new
                |> populateIndex shoppingLists
      }
    , Cmd.none
    )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SelectList l ->
            ( { model | screen = ShoppingList l }, Cmd.none )

        BackToListSelection ->
            ( { model | screen = ListSelection }, Cmd.none )

        CompleteItem item ->
            ( mapCurrentShoppingList (completeItem item) model, Cmd.none )

        AddItem item ->
            ( mapCurrentShoppingList (addItem item) model
                |> mapItemIndex (SimpleTextIndex.add item)
            , Cmd.none
            )

        OpenItemCreator ->
            case model.screen of
                ShoppingList list ->
                    let
                        focusSearchBar =
                            Browser.Dom.focus searchBarId
                                |> Task.attempt (\_ -> NoOp)
                    in
                    ( { model | screen = ItemCreation list "" }, focusSearchBar )

                _ ->
                    ( model, Cmd.none )

        OpenListCreator ->
            case model.screen of
                ListSelection ->
                    ( { model | screen = ListCreation "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEditedItem updatedItem ->
            case model.screen of
                ItemCreation list _ ->
                    ( { model | screen = ItemCreation list updatedItem }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UpdateEditedList updatedList ->
            case model.screen of
                ListCreation listName ->
                    ( { model | screen = ListCreation updatedList }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        AddList listName ->
            case model.screen of
                ListCreation _ ->
                    let
                        ( shoppingListName, shoppingList ) =
                            newShoppingList listName
                    in
                    ( { model
                        | screen = ShoppingList shoppingListName
                        , shoppingLists = Dict.insert listName shoppingList model.shoppingLists
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        allShoppingLists =
            Dict.values model.shoppingLists

        displayShoppingListWith : (ShoppingList -> Html Msg) -> ShoppingListName -> Html Msg
        displayShoppingListWith shoppingListView shoppingListName =
            Dict.get (shoppingListNameToString shoppingListName) model.shoppingLists
                |> Maybe.map shoppingListView
                |> Maybe.withDefault (ListSelection.listSelectionPageView allShoppingLists)
    in
    case model.screen of
        ListSelection ->
            ListSelection.listSelectionPageView allShoppingLists

        ListCreation l ->
            ListCreation.listCreationPageView allShoppingLists l

        ShoppingList l ->
            displayShoppingListWith ShoppingList.shoppingListPageView l

        ItemCreation l item ->
            displayShoppingListWith (ItemCreation.itemCreationPageView model.itemIndex item) l


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
