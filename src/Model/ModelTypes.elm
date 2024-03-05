module Model.ModelTypes exposing (Item, ItemState(..), normalizeItem)

import String.Normalize


type alias Item =
    String


normalizeItem : Item -> String
normalizeItem =
    String.toLower >> String.Normalize.removeDiacritics


type ItemState
    = Completed
    | Pending
