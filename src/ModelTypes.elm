module ModelTypes exposing (Item, ShoppingList, ShoppingListName, newShoppingList, normalizeItem, shoppingListNameFromString, shoppingListNameToString)

import String.Normalize


type alias Item =
    String


normalizeItem : Item -> String
normalizeItem =
    String.toLower >> String.Normalize.removeDiacritics


type ShoppingListName
    = ShoppingListName String


shoppingListNameToString : ShoppingListName -> String
shoppingListNameToString (ShoppingListName name) =
    name


shoppingListNameFromString : String -> ShoppingListName
shoppingListNameFromString name =
    ShoppingListName name


type alias ShoppingList =
    { name : ShoppingListName
    , completed : List Item
    , pending : List Item
    }


newShoppingList : String -> ( ShoppingListName, ShoppingList )
newShoppingList nameString =
    let
        name =
            shoppingListNameFromString nameString
    in
    ( name
    , { name = name
      , completed = []
      , pending = []
      }
    )
