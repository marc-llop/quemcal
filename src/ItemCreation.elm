module ItemCreation exposing (itemCreationPageView)

import Design exposing (colors)
import Element exposing (Element, fill, height, px, width)
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
        , Border.rounded 32
        , Element.padding 20
        , width fill
        , height (px 54)
        , Font.color colors.lime
        ]
        [ Input.search
            [ Background.color colors.grey
            , width fill
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


itemCreationView : List Item -> String -> Item -> Element Msg
itemCreationView items shoppingListName editedItem =
    Element.column [ Font.color colors.lime ]
        [ searchBar editedItem
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
