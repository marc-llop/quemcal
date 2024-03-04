module ItemCreation exposing (ItemCreationData, addItem, itemCreationPageView, openItemCreator, searchBarId, updateEditedItem)

import Design exposing (backButton, colors)
import Element exposing (Element, fill, height, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes
import Icons
import Model.ShoppingList as ShoppingList exposing (ItemPresence(..), ShoppingList, ShoppingListID, pendingItems, shoppingListID)
import ModelTypes exposing (Item)
import Msg exposing (Msg(..))
import SimpleTextIndex exposing (Index)


type alias ItemCreationData =
    { shoppingListId : ShoppingListID
    , itemInput : String
    , editedItem : Item
    , searchResults : List ( ItemPresence, Item )
    }


openItemCreator : ShoppingListID -> ItemCreationData
openItemCreator shoppingListId =
    { shoppingListId = shoppingListId
    , itemInput = ""
    , editedItem = ""
    , searchResults = []
    }


updateEditedItem : Index Item -> ShoppingList -> String -> ItemCreationData -> ItemCreationData
updateEditedItem itemIndex shoppingList editedItem data =
    { data
        | itemInput = editedItem
        , editedItem = editedItem
        , searchResults = searchItems itemIndex shoppingList editedItem
    }


addItem : Index Item -> ShoppingList -> ItemCreationData -> ItemCreationData
addItem itemIndex shoppingList data =
    { data
        | itemInput = ""
        , searchResults = searchItems itemIndex shoppingList data.editedItem
    }


withPresence : ShoppingList -> Item -> ( ItemPresence, Item )
withPresence shoppingList item =
    ( ShoppingList.contains item shoppingList, item )


searchItems : Index Item -> ShoppingList -> String -> List ( ItemPresence, Item )
searchItems itemIndex shoppingList searchQuery =
    SimpleTextIndex.search searchQuery itemIndex
        |> List.map (withPresence shoppingList)


itemText : String -> Element msg
itemText item =
    Element.el
        [ width fill
        , Font.color colors.lime
        ]
        (Element.text item)


itemRow : Item -> ItemPresence -> Element msg
itemRow item itemPresence =
    let
        icon =
            case itemPresence of
                NotPresent ->
                    Icons.plus

                PresentCompleted ->
                    Icons.check

                PresentPending ->
                    Icons.list
    in
    Element.row
        [ width fill
        , Element.padding 20
        , Element.spacing 15
        , Background.color colors.black
        ]
        [ Element.el
            [ width (px 32) ]
            (Element.html icon)
        , Element.el [] <| itemText item
        ]


itemView : Item -> ItemPresence -> Element Msg
itemView item itemPresence =
    Input.button
        [ width fill
        , Element.focused []
        ]
        { onPress = Just <| AddItem item
        , label = itemRow item itemPresence
        }


searchBarId : String
searchBarId =
    "search-bar"


searchBar : Item -> Element Msg
searchBar editedItem =
    Element.row
        [ Background.color colors.grey
        , Border.rounded 22
        , Element.padding 20
        , width fill
        , height (px 44)
        , Font.color colors.lime
        ]
        [ Input.search
            [ Background.color colors.grey
            , width fill
            , height (px 44)
            , Element.focused []
            , Border.width 0
            , Font.alignLeft
            , Element.centerY
            , Input.focusedOnLoad
            , Element.htmlAttribute (Html.Attributes.id searchBarId)
            ]
            { onChange = UpdateEditedItem
            , text = editedItem
            , placeholder = Nothing
            , label = Input.labelHidden "New item"
            }
        , Element.el [ width (px 32) ] (Element.html Icons.search)
        ]


headerView : ShoppingListID -> Item -> Element Msg
headerView shoppingListID editedItem =
    Element.row
        [ Background.color colors.purple
        , width fill
        , height (px 64)
        , Font.color colors.lime
        ]
        [ backButton (SelectList shoppingListID)
        , searchBar editedItem
        , Element.el [ width (px 20) ] Element.none
        ]


itemCreationView : List ( ItemPresence, Item ) -> ShoppingListID -> Item -> Element Msg
itemCreationView items shoppingListID editedItem =
    Element.column
        [ Font.color colors.lime
        , width fill
        , height fill
        , Background.color colors.black
        ]
        [ headerView shoppingListID editedItem
        , Element.el
            [ width fill
            , height fill
            , Background.color colors.black
            , Element.scrollbarY
            ]
            (Keyed.column
                [ width fill
                , height shrink
                , Background.color colors.grey
                , Element.spacing 1
                ]
                (List.map (\( itemPresence, item ) -> ( item, itemView item itemPresence )) items)
            )
        ]


itemCreationPageView : ItemCreationData -> Html Msg
itemCreationPageView data =
    Element.layout
        [ Background.color colors.black ]
        (itemCreationView data.searchResults data.shoppingListId data.editedItem)
