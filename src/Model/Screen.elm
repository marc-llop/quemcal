module Model.Screen exposing (..)

import Model.ModelTypes exposing (Item)
import Model.ShoppingList exposing (ItemPresence(..), ShoppingListID, shoppingListID)
import Msg exposing (Msg(..))


type Screen
    = ListSelection
    | ListCreation String
    | ShoppingList ShoppingListID
    | ItemCreation ItemCreationData


type alias ItemCreationData =
    { shoppingListId : ShoppingListID
    , itemInput : String
    , editedItem : Item
    , searchResults : List ( ItemPresence, Item )
    }


initialItemCreatorState : ShoppingListID -> ItemCreationData
initialItemCreatorState shoppingListId =
    { shoppingListId = shoppingListId
    , itemInput = ""
    , editedItem = ""
    , searchResults = []
    }
