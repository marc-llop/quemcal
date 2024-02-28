module Model.ShoppingList exposing (ItemState(..), ShoppingList, ShoppingListName, addItem, completedItems, listProgress, newShoppingList, pendingItems, shoppingListName, shoppingListNameToString, testData, toggleItem)

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
    , total : Int
    , completed : Int
    }


type ShoppingListName
    = ShoppingListName String


shoppingListName : ShoppingList -> ShoppingListName
shoppingListName (ShoppingList { name }) =
    name


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
        , total = 0
        , completed = 0
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
toggleItem item (ShoppingList internals) =
    let
        newCompleted =
            case Dict.get item internals.items of
                Nothing ->
                    internals.completed

                Just Completed ->
                    internals.completed - 1

                Just Pending ->
                    internals.completed + 1

        toggleExisting =
            Maybe.map toggleItemState
    in
    ShoppingList
        { internals
            | items = Dict.update item toggleExisting internals.items
            , completed = newCompleted
        }


addItem : Item -> ShoppingList -> ShoppingList
addItem item (ShoppingList internals) =
    let
        ( newCompleted, newTotal ) =
            case Dict.get item internals.items of
                Nothing ->
                    ( internals.completed, internals.total + 1 )

                Just Completed ->
                    ( internals.completed - 1, internals.total )

                Just Pending ->
                    ( internals.completed, internals.total )
    in
    ShoppingList
        { internals
            | items = Dict.insert item Pending internals.items
            , completed = newCompleted
            , total = newTotal
        }


listProgress : ShoppingList -> { totalAmount : Int, completedAmount : Int }
listProgress (ShoppingList { total, completed }) =
    { totalAmount = total, completedAmount = completed }


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
                    , total = List.length sl.pending + List.length sl.completed
                    , completed = List.length sl.completed
                    }
                )
            )
        |> Dict.fromList
