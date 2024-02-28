module Msg exposing (Msg(..))

import Model.ShoppingList exposing (ShoppingListName)


type Msg
    = NoOp
    | SelectList ShoppingListName
    | BackToListSelection
    | CompleteItem String
    | AddItem String
    | OpenItemCreator
    | OpenListCreator
    | UpdateEditedItem String
    | UpdateEditedList String
    | AddList String
