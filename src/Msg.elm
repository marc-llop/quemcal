module Msg exposing (Msg(..))

import LongTouch exposing (LongTouchMsg)
import Model.ShoppingList exposing (ShoppingListID)
import ModelTypes exposing (Item)


type Msg
    = NoOp
    | SelectList ShoppingListID
    | BackToListSelection
    | CompleteItem Item
    | AddItem String
    | DeleteItem Item
    | OpenItemCreator
    | OpenListCreator
    | UpdateEditedItem String
    | UpdateEditedList String
    | AddList String
    | LongTouch LongTouchMsg
