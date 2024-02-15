module ShoppingList exposing (Item, ShoppingList, shoppingListView)

import Design exposing (colors, fabMargin)
import Element exposing (Element, fill, height, px, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Icons
import Msg exposing (Msg(..))


type alias Item =
    String


type alias ShoppingList =
    { name : String
    , completed : List Item
    , pending : List Item
    }


type ItemState
    = Completed
    | Pending


itemText : ItemState -> Item -> Element msg
itemText itemState item =
    let
        color =
            case itemState of
                Completed ->
                    [ Font.color colors.lime ]

                Pending ->
                    [ Font.color colors.lightLime, Font.glow colors.lime 2.0 ]
    in
    Element.el
        (width fill :: color)
        (text item)


itemCheckbox : ItemState -> Element Msg
itemCheckbox itemState =
    let
        ( color, icon ) =
            case itemState of
                Completed ->
                    ( colors.lime, Icons.checkSquare )

                Pending ->
                    ( colors.lightLime, Icons.square )
    in
    Element.el
        [ width (px 32)
        , Font.color color
        , Font.glow colors.lime 2.0
        ]
        (Element.html icon)


itemView : ItemState -> Item -> Element Msg
itemView itemState item =
    let
        backgroundColor =
            case itemState of
                Completed ->
                    colors.black

                Pending ->
                    colors.purple

        itemRow =
            Element.row
                [ width fill
                , Element.padding 20
                , Background.color backgroundColor
                , Element.spacing 15
                ]
                [ itemCheckbox itemState
                , Element.el [] <| itemText itemState item
                ]
    in
    Input.button
        [ width fill ]
        { onPress =
            case itemState of
                Completed ->
                    Just (AddItem item)

                Pending ->
                    Just (CompleteItem item)
        , label = itemRow
        }


listHeaderView : String -> Element Msg
listHeaderView listName =
    let
        headerIcon =
            Element.el
                [ width (px 32)
                , Font.color colors.lightLime
                ]
                (Element.html Icons.arrowLeft)
    in
    Element.row
        [ Element.paddingXY 20 20
        , spacing 20
        , Background.color colors.purple
        , width fill
        ]
        [ Input.button []
            { onPress = Just BackToListSelection
            , label = headerIcon
            }
        , Element.el
            [ Font.color colors.lightLime
            , Font.bold
            ]
            (text listName)
        ]


shoppingListView : ShoppingList -> Html Msg
shoppingListView { name, completed, pending } =
    let
        backgroundColor state =
            case state of
                Pending ->
                    colors.lightGrey

                Completed ->
                    colors.grey

        listColumn state =
            Element.column
                [ width fill
                , Background.color <| backgroundColor state
                , spacing 1
                ]
    in
    Element.layout [ Background.color colors.black ]
        (Element.column [ width fill, height fill ]
            [ listHeaderView name
            , Element.column
                [ height fill, width fill, spacing 1, Element.scrollbarX ]
                [ listColumn Pending
                    (List.map (itemView Pending) pending)
                , listColumn Completed
                    (List.map (itemView Completed) completed)
                , fabMargin
                ]
            ]
        )
