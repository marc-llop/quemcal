module Model.ShoppingList exposing (ItemState(..), ShoppingList, ShoppingListID, addItem, completedItems, idToString, listProgress, newShoppingList, pendingItems, shoppingListID, shoppingListName, testData, toggleItem)

import Dict exposing (Dict)
import Html.Attributes exposing (id)
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
    { id : ShoppingListID
    , items : Dict Item ItemState
    , total : Int
    , completed : Int
    }


type ShoppingListID
    = ShoppingListID String


shoppingListID : ShoppingList -> ShoppingListID
shoppingListID (ShoppingList { id }) =
    id


idToString : ShoppingListID -> String
idToString (ShoppingListID name) =
    name


shoppingListName : ShoppingList -> String
shoppingListName (ShoppingList { id }) =
    case id of
        ShoppingListID name ->
            name


shoppingListIDFromString : String -> ShoppingListID
shoppingListIDFromString name =
    ShoppingListID name


newShoppingList : String -> ( ShoppingListID, ShoppingList )
newShoppingList nameString =
    let
        id =
            shoppingListIDFromString nameString
    in
    ( id
    , ShoppingList
        { id = id
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
                    { id = shoppingListIDFromString sl.name
                    , items = twoListsToDict sl
                    , total = List.length sl.pending + List.length sl.completed
                    , completed = List.length sl.completed
                    }
                )
            )
        |> Dict.fromList
