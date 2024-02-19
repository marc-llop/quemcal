module ItemCreation exposing (itemCreationPageView)

import Design exposing (colors)
import Element exposing (Element, height, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Msg exposing (Msg(..))
import ShoppingList exposing (Item)
import SimpleTextIndex exposing (Index)


itemCreationView : List Item -> String -> Item -> Element Msg
itemCreationView items shoppingListName editedItem =
    Element.column [ Font.color colors.lime ]
        [ Input.search []
            { onChange = UpdateEditedItem
            , text = editedItem
            , placeholder = Nothing
            , label = Input.labelHidden "New item"
            }
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
