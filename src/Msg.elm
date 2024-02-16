module Msg exposing (Msg(..))


type Msg
    = SelectList String
    | BackToListSelection
    | CompleteItem String
    | AddItem String
    | OpenItemCreator
