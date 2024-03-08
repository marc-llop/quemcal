module Model.Screen exposing (..)

import Browser.Dom
import ItemCreation exposing (ItemCreationData)
import ListCreation
import Model.ShoppingList exposing (ShoppingList, ShoppingListID)
import Msg exposing (Msg(..))
import Url exposing (Url)
import Url.Parser exposing (map, s, string, top)


type Screen
    = ListSelection
    | ListCreation String
    | ShoppingList ShoppingListID
    | ItemCreation ItemCreationData


focusOnLoad : String -> Cmd Msg
focusOnLoad elementId =
    Browser.Dom.focus elementId
        |> Task.attempt (\_ -> NoOp)


onLoadCommand : Screen -> Cmd Msg
onLoadCommand screen =
    case screen of
        ListCreation _ ->
            focusOnLoad ListCreation.listNameInputId

        ItemCreation _ ->
            focusOnLoad ItemCreation.searchBarId

        _ ->
            Cmd.none


screenToUrl : Screen -> Url
screenToUrl screen =
    case screen of
        ListSelection ->
            Url.Builder.absolute [] []

        ListCreation _ ->
            Url.Builder.absolute [ "nova_llista" ] []

        ShoppingList id ->
            Url.Builder.absolute [ "llista", idToString id ] []

        ItemCreation { shoppingListId } ->
            Url.Builder.absolute [ "llista", idToString shoppingListId, "cerca" ] []


urlToScreen : Dict String ShoppingList -> Url -> Screen
urlToScreen shoppingLists url =
    let
        getShoppingListScreen : String -> Maybe Screen
        getShoppingListScreen name =
            Dict.get name shoppingLists
                |> Maybe.map shoppingListID

        getShoppingListOnRoot : String -> Screen
        getShoppingListOnRoot name =
            getShoppingListScreen name
                |> Maybe.map ShoppingList
                |> Maybe.withDefault ListSelection

        getShoppingListOnSearch : String -> Screen
        getShoppingListOnSearch name =
            getShoppingListScreen name
                |> Maybe.map ItemCreation.openItemCreator
                |> Maybe.withDefault ListSelection

        parseUrl =
            Url.Parse.oneOf
                [ top |> map ListSelection
                , s "nova_llista" |> map (ListCreation "")
                , s "llista" </> string |> map getShoppingListOnRoot
                , s "llista" </> string </> s "cerca" |> map getShoppingListOnSearch
                ]
    in
    Url.Parse.parse parseUrl url
        |> Maybe.withDefault ListSelection
