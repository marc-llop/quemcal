module Router exposing (..)

import Browser.Dom
import Dict exposing (Dict)
import Model.ModelTypes exposing (Item)
import Model.Screen as Screen exposing (ItemCreationData, Screen(..), initialItemCreatorState)
import Model.ShoppingList exposing (ItemPresence(..), ShoppingList, ShoppingListID, idToString, shoppingListID)
import Msg exposing (Msg(..))
import Url exposing (Url, percentDecode, percentEncode)
import Url.Builder
import Url.Parser exposing (..)


type Route
    = ListSelection
    | ListCreation
    | ShoppingList String
    | ItemCreation String


defaultRoute : Route
defaultRoute =
    ListSelection


defaultScreen : Screen
defaultScreen =
    Screen.ListSelection


urlToRoute : Url -> Route
urlToRoute url =
    let
        parseUrl =
            Url.Parser.oneOf
                [ top |> map ListSelection
                , s "nova_llista" |> map ListCreation
                , s "llista" </> string |> map ShoppingList
                , s "llista" </> string </> s "cerca" |> map ItemCreation
                ]
    in
    Url.Parser.parse parseUrl url
        |> Maybe.withDefault defaultRoute


routeToScreen : Dict String ShoppingList -> Route -> Screen
routeToScreen shoppingLists route =
    let
        nameToMaybeID : String -> Maybe ShoppingListID
        nameToMaybeID name =
            percentDecode name
                |> Maybe.andThen (\decodedName -> Dict.get decodedName shoppingLists)
                |> Maybe.map shoppingListID

        getShoppingListScreen : String -> Screen
        getShoppingListScreen name =
            nameToMaybeID name
                |> Maybe.map Screen.ShoppingList
                |> Maybe.withDefault defaultScreen

        getItemCreationScreen : String -> Screen
        getItemCreationScreen name =
            nameToMaybeID name
                |> Maybe.map initialItemCreatorState
                |> Maybe.map Screen.ItemCreation
                |> Maybe.withDefault defaultScreen
    in
    case route of
        ListSelection ->
            Screen.ListSelection

        ListCreation ->
            Screen.ListCreation ""

        ShoppingList name ->
            getShoppingListScreen name

        ItemCreation name ->
            getItemCreationScreen name


urlToScreen : Dict String ShoppingList -> Url -> Screen
urlToScreen shoppingLists =
    urlToRoute >> routeToScreen shoppingLists


stringToRoute : String -> Route
stringToRoute urlString =
    Url.fromString urlString
        |> Maybe.map (\url -> urlToRoute <| Debug.log "stringToRoute" url)
        |> Maybe.withDefault ListSelection


goToListSelection : Msg
goToListSelection =
    Url.Builder.absolute [] []
        |> Navigate


goToListCreation : Msg
goToListCreation =
    Url.Builder.absolute [ "nova_llista" ] []
        |> Navigate


goToShoppingList : ShoppingListID -> Msg
goToShoppingList id =
    Navigate (shoppingListUrl id)


shoppingListUrl : ShoppingListID -> String
shoppingListUrl id =
    Url.Builder.absolute [ "llista", percentEncode <| idToString id ] []


goToItemCreation : ShoppingListID -> Msg
goToItemCreation id =
    Url.Builder.absolute [ "llista", idToString id, "cerca" ] []
        |> Navigate
