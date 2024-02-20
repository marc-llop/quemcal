module ModelTypes exposing (Item, ShoppingList, ShoppingListName, shoppingListNameFromString, shoppingListNameToString)


type alias Item =
    String


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
