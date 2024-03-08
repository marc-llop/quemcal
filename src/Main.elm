module Main exposing (main)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Html exposing (Html)
import ItemCreation exposing (ItemCreationData, searchBarId)
import ListCreation exposing (listNameInputId)
import ListSelection
import LongTouch exposing (LongTouchModel)
import Model.ModelTypes exposing (Item, ItemState(..))
import Model.Screen as Screen exposing (Screen(..))
import Model.ShoppingList as ShoppingList exposing (ShoppingList, ShoppingListID, completedItems, idToString, newShoppingList, pendingItems, shoppingListName, testData, toggleItem)
import Msg exposing (Msg(..))
import Platform.Cmd as Cmd
import ShoppingListPage
import SimpleTextIndex exposing (Index)
import Task
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = Navigate
        , onUrlRequest = onUrlRequest
        }


onUrlRequest : UrlRequest -> Msg
onUrlRequest urlRequest =
    case urlRequest of
        Internal url ->
            Navigate url

        External _ ->
            NoOp


type alias Flags =
    ()


type alias Model =
    { screen : Screen
    , shoppingLists : Dict String ShoppingList
    , itemIndex : Index Item
    , longTouch : LongTouchModel
    , key : Key
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


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        shoppingLists =
            testData
    in
    ( { screen = Screen.urlToScreen shoppingLists url
      , shoppingLists = shoppingLists
      , itemIndex =
            SimpleTextIndex.config
                { ref = itemToString
                , fields = [ itemToString ]
                , normalize = Model.ModelTypes.normalizeItem
                }
                |> SimpleTextIndex.new
                |> populateIndex testData
      , longTouch = LongTouch.initLongTouch
      , key = key
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


mapItemCreationScreen : (Index Item -> ShoppingList -> ItemCreationData -> ItemCreationData) -> Model -> ( Model, Cmd msg )
mapItemCreationScreen mapper model =
    case model.screen of
        ItemCreation screenData ->
            let
                dictKey =
                    idToString screenData.shoppingListId

                maybeShoppingList =
                    Dict.get dictKey model.shoppingLists
            in
            maybeShoppingList
                |> Maybe.map (\shoppingList -> mapper model.itemIndex shoppingList screenData)
                |> Maybe.map (\newScreenData -> ( { model | screen = ItemCreation newScreenData }, Cmd.none ))
                |> Maybe.withDefault ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


mapItemIndex : (Index Item -> Index Item) -> Model -> Model
mapItemIndex mapper model =
    { model | itemIndex = mapper model.itemIndex }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Navigate url ->
            let
                newScreen =
                    Screen.urlToScreen model.shoppingLists url
            in
            ( { model | screen = newScreen }, Screen.onLoadCommand newScreen )

        AddItem item ->
            model
                |> mapItemIndex (SimpleTextIndex.add item)
                |> mapCurrentShoppingList (ShoppingList.addItem item)
                |> mapItemCreationScreen ItemCreation.addItem

        DeleteItem item ->
            model
                |> mapCurrentShoppingList (ShoppingList.deleteItem item)
                |> mapItemCreationScreen ItemCreation.deleteItem

        UpdateEditedItem updatedItem ->
            mapItemCreationScreen (ItemCreation.updateEditedItem updatedItem) model

        UpdateEditedList updatedList ->
            case model.screen of
                ListCreation _ ->
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

        LongTouch longTouchMsg ->
            let
                shouldDeleteItem =
                    LongTouch.shouldDeleteItem longTouchMsg model.longTouch

                maybeItem =
                    LongTouch.touchEndInfo longTouchMsg model.longTouch

                mapLongTouch : (LongTouchModel -> LongTouchModel) -> Model -> Model
                mapLongTouch mapper a =
                    { a | longTouch = mapper a.longTouch }

                modelWithToggledItem item =
                    model
                        |> mapCurrentShoppingList (ShoppingList.toggleItem item)

                modelWithDeletedItem item =
                    model
                        |> mapCurrentShoppingList (ShoppingList.deleteItem item)

                modelWithUpdatedItem =
                    case ( maybeItem, shouldDeleteItem ) of
                        ( Nothing, _ ) ->
                            model

                        ( Just item, False ) ->
                            modelWithToggledItem item

                        ( Just item, True ) ->
                            modelWithDeletedItem item
            in
            modelWithUpdatedItem
                |> mapLongTouch (LongTouch.updateLongTouch longTouchMsg)
                |> (\newModel -> ( newModel, Cmd.none ))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map LongTouch (LongTouch.longTouchSubscription model.longTouch)


view : Model -> Browser.Document Msg
view model =
    { title = "Quèmcal"
    , body = [ viewHtml model ]
    }


viewHtml : Model -> Html Msg
viewHtml model =
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
            ItemCreation.itemCreationPageView data
