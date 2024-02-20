module Msg exposing (Msg(..))

import ModelTypes exposing (ShoppingListName)


type Msg
    = NoOp
    | SelectList ShoppingListName
    | BackToListSelection
    | CompleteItem String
    | AddItem String
    | OpenItemCreator
    | OpenListCreator
    | UpdateEditedItem String
