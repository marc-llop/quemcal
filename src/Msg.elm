module Msg exposing (Msg(..))

import LongTouch exposing (LongTouchMsg)
import Model.ModelTypes exposing (Item)
import Url exposing (Url)


type Msg
    = NoOp
    | LoadScreen Url
    | Navigate String
      -- | SelectList ShoppingListID
      -- | BackToListSelection
    | AddItem String
    | DeleteItem Item
      -- | OpenItemCreator
      -- | OpenListCreator
    | UpdateEditedItem String
    | UpdateEditedList String
    | AddList String
    | LongTouch (LongTouchMsg Item)
