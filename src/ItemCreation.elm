module ItemCreation exposing (itemCreationPageView)

import Design exposing (backButton, colors)
import Element exposing (Element, fill, height, maximum, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Icons
import Msg exposing (Msg(..))
import ShoppingList exposing (Item)
import SimpleTextIndex exposing (Index)


itemText : String -> Element msg
itemText item =
    Element.el
        [ width fill
        , Font.color colors.lime
        ]
        (Element.text item)


itemRow : Item -> Element msg
itemRow item =
    Element.row
        [ width fill
        , Element.padding 20
        , Element.spacing 15
        , Background.color colors.black
        ]
        [ Element.el
            [ width (px 32) ]
            (Element.html Icons.plus)
        , Element.el [] <| itemText item
        ]


itemView : Item -> Element Msg
itemView item =
    Input.button
        [ width fill
        , Element.focused []
        ]
        { onPress = Just <| AddItem item
        , label = itemRow item
        }


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
        , Background.color colors.black
        ]
        [ headerView shoppingListName editedItem
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
                (List.map (\i -> ( i, itemView i )) items)
            )
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
