module Msg exposing (Msg(..))

import Model.ShoppingList exposing (ShoppingListID)


type Msg
    = NoOp
    | SelectList ShoppingListID
    | BackToListSelection
    | CompleteItem String
    | AddItem String
    | OpenItemCreator
    | OpenListCreator
    | UpdateEditedItem String
    | UpdateEditedList String
    | AddList String
