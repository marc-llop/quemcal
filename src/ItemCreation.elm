module ItemCreation exposing (ItemCreationData, addItem, deleteItem, itemCreationPageView, openItemCreator, searchBarId, updateEditedItem)

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
import Model.ModelTypes exposing (Item)
import Model.ShoppingList as ShoppingList exposing (ItemPresence(..), ShoppingList, ShoppingListID, pendingItems, shoppingListID)
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


updateEditedItem : String -> Index Item -> ShoppingList -> ItemCreationData -> ItemCreationData
updateEditedItem editedItem itemIndex shoppingList data =
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


deleteItem : Index Item -> ShoppingList -> ItemCreationData -> ItemCreationData
deleteItem itemIndex shoppingList data =
    { data
        | searchResults = searchItems itemIndex shoppingList data.editedItem
    }


searchItems : Index Item -> ShoppingList -> String -> List ( ItemPresence, Item )
searchItems itemIndex shoppingList searchQuery =
    let
        withPresence : Item -> ( ItemPresence, Item )
        withPresence item =
            ( ShoppingList.contains item shoppingList, item )

        searchResults =
            SimpleTextIndex.search searchQuery itemIndex
                |> List.filter (\result -> result /= searchQuery)
                |> List.map withPresence
    in
    if searchQuery == "" then
        searchResults

    else
        withPresence searchQuery :: searchResults


itemText : String -> ItemPresence -> Element msg
itemText item itemPresence =
    Element.el
        (if itemPresence == PresentPending then
            [ Font.alignLeft ]

         else
            [ Font.alignLeft, Font.glow colors.lime 2.0 ]
        )
        (Element.text item)


deleteButton : Item -> Element Msg
deleteButton item =
    Input.button
        [ width (px 64)
        , height fill
        , Element.padding 20
        , Font.color colors.red
        , Element.focused []
        ]
        { onPress = Just (DeleteItem item)
        , label = Element.html Icons.trash2
        }


itemRow : Item -> ItemPresence -> Element Msg
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

        iconElement =
            Element.el
                [ width (px 64)
                , height fill
                , Element.padding 20
                ]
                (Element.html icon)

        textBox =
            Element.el
                [ width fill
                , Element.paddingEach
                    { top = 20
                    , bottom = 20
                    , left = 0
                    , right =
                        if itemPresence == PresentPending then
                            0

                        else
                            20
                    }
                ]
                (itemText item itemPresence)
    in
    Element.row
        [ width fill
        , Background.color colors.black
        ]
        [ iconElement
        , textBox
        ]


itemView : Item -> ItemPresence -> Element Msg
itemView item itemPresence =
    let
        hasDeleteButton =
            itemPresence == PresentPending

        color =
            if hasDeleteButton then
                colors.lime

            else
                colors.lightLime

        deleteButtonIfNeeded =
            if hasDeleteButton then
                deleteButton item

            else
                Element.none

        addEvent =
            if hasDeleteButton then
                Nothing

            else
                Just (AddItem item)
    in
    Element.row
        [ width fill
        , Background.color colors.black
        , Font.color color
        ]
        [ Input.button
            [ width fill
            , Element.focused []
            ]
            { onPress = addEvent
            , label = itemRow item itemPresence
            }
        , deleteButtonIfNeeded
        ]


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
        (itemCreationView data.searchResults data.shoppingListId data.itemInput)
