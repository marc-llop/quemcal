module ItemCreation exposing (itemCreationPageView)

import Design exposing (backButton, colors)
import Element exposing (Element, fill, height, maximum, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Icons
import Msg exposing (Msg(..))
import ShoppingList exposing (Item)
import SimpleTextIndex exposing (Index)


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
            ]
            { onChange = UpdateEditedItem
            , text = editedItem
            , placeholder = Nothing
            , label = Input.labelHidden "New item"
            }
        , Element.el [ width (px 32) ] (Element.html Icons.search)
        ]


headerView : String -> Item -> Element Msg
headerView shoppingListName editedItem =
    Element.row
        [ Background.color colors.purple
        , width fill
        , height (px 64)
        , Font.color colors.lime
        ]
        [ backButton (SelectList shoppingListName)
        , searchBar editedItem
        , Element.el [ width (px 20) ] Element.none
        ]


itemCreationView : List Item -> String -> Item -> Element Msg
itemCreationView items shoppingListName editedItem =
    Element.column
        [ Font.color colors.lime
        , width fill
        , height fill
        ]
        [ headerView shoppingListName editedItem
        , Element.column []
            (List.map Element.text items)
        ]


itemCreationPageView : Index Item -> String -> Item -> Html Msg
itemCreationPageView itemIndex shoppingListName editedItem =
    let
        items : List Item
        items =
            SimpleTextIndex.search editedItem itemIndex
    in
    Element.layout
        [ Background.color colors.black ]
        (itemCreationView items shoppingListName editedItem)
