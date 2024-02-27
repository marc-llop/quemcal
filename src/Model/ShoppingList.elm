module Model.ShoppingList exposing (ItemState(..), ShoppingList, ShoppingListName, addItem, completedItems, newShoppingList, pendingItems, shoppingListNameToString, toggleItem)

import Dict exposing (Dict)
import ModelTypes exposing (Item)


type ShoppingList
    = ShoppingList Internals


type ItemState
    = Completed
    | Pending


toggleItemState : ItemState -> ItemState
toggleItemState state =
    case state of
        Pending ->
            Completed

        Completed ->
            Pending


type alias Internals =
    { name : ShoppingListName
    , items : Dict Item ItemState
    }


type ShoppingListName
    = ShoppingListName String


shoppingListNameToString : ShoppingListName -> String
shoppingListNameToString (ShoppingListName name) =
    name


shoppingListNameFromString : String -> ShoppingListName
shoppingListNameFromString name =
    ShoppingListName name


newShoppingList : String -> ( ShoppingListName, ShoppingList )
newShoppingList nameString =
    let
        name =
            shoppingListNameFromString nameString
    in
    ( name
    , ShoppingList
        { name = name
        , items = Dict.empty
        }
    )


itemsInState : ItemState -> ShoppingList -> List Item
itemsInState state (ShoppingList { items }) =
    Dict.toList items
        |> List.filterMap
            (\( item, itemState ) ->
                if itemState == state then
                    Just item

                else
                    Nothing
            )


completedItems : ShoppingList -> List Item
completedItems =
    itemsInState Completed


pendingItems : ShoppingList -> List Item
pendingItems =
    itemsInState Pending


mapItems : (Dict Item ItemState -> Dict Item ItemState) -> ShoppingList -> ShoppingList
mapItems mapper (ShoppingList internals) =
    ShoppingList { internals | items = mapper internals.items }


toggleItem : Item -> ShoppingList -> ShoppingList
toggleItem item shoppingList =
    let
        toggleExisting =
            Maybe.map toggleItemState
    in
    mapItems (Dict.update item toggleExisting) shoppingList


addItem : Item -> ShoppingList -> ShoppingList
addItem item shoppingList =
    let
        addAsPending =
            Maybe.andThen (always (Just Pending))
    in
    mapItems (Dict.update item addAsPending) shoppingList


marketShoppingList =
    { name = "Market"
    , pending =
        [ "Cockles"
        , "Squid"
        , "Salmon"
        , "Haddock"
        , "Cod"
        ]
    , completed =
        [ "Potatoes"
        , "Cucumbers"
        , "Bananas"
        , "Tomatoes"
        , "Onions"
        , "Carrots"
        , "Spinachs"
        ]
    }


twoListsToDict : { a | completed : List Item, pending : List Item } -> Dict Item ItemState
twoListsToDict { completed, pending } =
    let
        completedPairs =
            List.map (\s -> ( s, Completed )) completed

        pendingPairs =
            List.map (\s -> ( s, Pending )) pending

        pairs =
            completedPairs ++ pendingPairs
    in
    Dict.fromList pairs


testData : Dict String ShoppingList
testData =
    [ marketShoppingList
    , { name = "Groceries"
      , pending = [ "Cookies", "Bread", "Milk" ]
      , completed = [ "Pizza", "Frankfurts" ]
      }
    , { name = "Don't put preservatives in food, it's gross.", pending = [ "Tuna", "Olives", "Asparagus", "Pickled onions" ], completed = [] }
    , { name = "Completed list", pending = [], completed = [ "Style ListSelection view" ] }
    , { name = "Empty list", pending = [], completed = [] }
    , { name = "Half-done list", pending = [ "Make the app work" ], completed = [ "Make some screens" ] }
    ]
        |> List.map
            (\sl ->
                ( sl.name
                , ShoppingList
                    { name = shoppingListNameFromString sl.name
                    , items = twoListsToDict sl
                    }
                )
            )
        |> Dict.fromList
