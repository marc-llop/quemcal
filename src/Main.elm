module Main exposing (main)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Html exposing (Html)
import ItemCreation exposing (ItemCreationData, searchBarId)
import ListCreation exposing (listNameInputId)
import ListSelection
import Model.ShoppingList exposing (ShoppingList, ShoppingListID, addItem, completedItems, idToString, newShoppingList, pendingItems, shoppingListName, testData, toggleItem)
import ModelTypes exposing (Item)
import Msg exposing (Msg(..))
import Platform.Cmd as Cmd
import ShoppingListPage
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
    | ShoppingList ShoppingListID
    | ItemCreation ItemCreationData


type alias Model =
    { screen : Screen
    , shoppingLists : Dict String ShoppingList
    , itemIndex : Index Item
    }


shoppingListsToItems : List ShoppingList -> List Item
shoppingListsToItems lists =
    List.concatMap
        (\shoppingList -> completedItems shoppingList ++ pendingItems shoppingList)
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
      , shoppingLists = testData
      , itemIndex =
            SimpleTextIndex.config
                { ref = itemToString
                , fields = [ itemToString ]
                , normalize = ModelTypes.normalizeItem
                }
                |> SimpleTextIndex.new
                |> populateIndex testData
      }
    , Cmd.none
    )


mapShoppingList : ShoppingListID -> (ShoppingList -> ShoppingList) -> Model -> Model
mapShoppingList listID mapper model =
    { model
        | shoppingLists = Dict.update (idToString listID) (Maybe.map mapper) model.shoppingLists
    }


mapCurrentShoppingList : (ShoppingList -> ShoppingList) -> Model -> Model
mapCurrentShoppingList mapper model =
    case model.screen of
        ShoppingList list ->
            mapShoppingList list mapper model

        ItemCreation { shoppingListId } ->
            mapShoppingList shoppingListId mapper model

        _ ->
            model


mapModelWithShoppingList : (ShoppingList -> ( Model, Cmd msg )) -> ShoppingListID -> Model -> ( Model, Cmd msg )
mapModelWithShoppingList mapModel shoppingListId model =
    let
        dictKey =
            idToString shoppingListId

        maybeShoppingList =
            Dict.get dictKey model.shoppingLists
    in
    case maybeShoppingList of
        Nothing ->
            ( model, Cmd.none )

        Just shoppingList ->
            mapModel shoppingList


mapItemIndex : (Index Item -> Index Item) -> Model -> Model
mapItemIndex mapper model =
    { model | itemIndex = mapper model.itemIndex }


cleanItemSearchInput : Model -> Model
cleanItemSearchInput model =
    case model.screen of
        ItemCreation itemCreationData ->
            { model
                | screen =
                    ItemCreation { itemCreationData | itemInput = "" }
            }

        _ ->
            model


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
            ( mapCurrentShoppingList (toggleItem item) model, Cmd.none )

        AddItem item ->
            ( mapCurrentShoppingList (addItem item) model
                |> mapItemIndex (SimpleTextIndex.add item)
                |> cleanItemSearchInput
            , Cmd.none
            )

        OpenItemCreator ->
            case model.screen of
                ShoppingList listId ->
                    let
                        focusSearchBar =
                            Browser.Dom.focus searchBarId
                                |> Task.attempt (\_ -> NoOp)

                        itemCreationData =
                            ItemCreation.openItemCreator listId
                    in
                    ( { model | screen = ItemCreation itemCreationData }, focusSearchBar )

                _ ->
                    ( model, Cmd.none )

        OpenListCreator ->
            case model.screen of
                ListSelection ->
                    let
                        focusListNameInput =
                            Browser.Dom.focus listNameInputId
                                |> Task.attempt (\_ -> NoOp)
                    in
                    ( { model | screen = ListCreation "" }, focusListNameInput )

                _ ->
                    ( model, Cmd.none )

        UpdateEditedItem updatedItem ->
            case model.screen of
                ItemCreation screenData ->
                    let
                        updateEditedItem : ShoppingList -> ( Model, Cmd Msg )
                        updateEditedItem shoppingList =
                            ItemCreation.updateEditedItem model.itemIndex shoppingList updatedItem screenData
                                |> (\newItemCreationData ->
                                        ( { model | screen = ItemCreation newItemCreationData }, Cmd.none )
                                   )
                    in
                    mapModelWithShoppingList updateEditedItem screenData.shoppingListId model

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

        displayShoppingListWith : (ShoppingList -> Html Msg) -> ShoppingListID -> Html Msg
        displayShoppingListWith shoppingListView shoppingListID =
            Dict.get (idToString shoppingListID) model.shoppingLists
                |> Maybe.map shoppingListView
                |> Maybe.withDefault (ListSelection.listSelectionPageView allShoppingLists)
    in
    case model.screen of
        ListSelection ->
            ListSelection.listSelectionPageView allShoppingLists

        ListCreation l ->
            ListCreation.listCreationPageView allShoppingLists l

        ShoppingList l ->
            displayShoppingListWith ShoppingListPage.shoppingListPageView l

        ItemCreation data ->
            displayShoppingListWith (ItemCreation.itemCreationPageView model.itemIndex data) data.shoppingListId
