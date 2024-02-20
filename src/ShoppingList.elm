module ShoppingList exposing (Item, ShoppingList, shoppingListPageView, shoppingListView)

import Design exposing (backButton, colors, fabMargin, floatingActionButton)
import Element exposing (Element, fill, height, px, shrink, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Html.Attributes
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
        [ width fill
        , Element.focused []
        ]
        { onPress =
            case itemState of
                Completed ->
                    Just (AddItem item)

                Pending ->
                    Just (CompleteItem item)
        , label = itemRow
        }



-- Stolen from https://github.com/rob-sokolowski/site/pull/52.
-- Check issue for more information: https://github.com/mdgriffith/elm-ui/issues/112


textWithEllipsis : String -> Element Msg
textWithEllipsis displayText =
    Element.html
        (Html.div
            [ Html.Attributes.style "text-overflow" "ellipsis"
            , Html.Attributes.style "overflow" "hidden"
            ]
            [ Html.text displayText ]
        )


listHeaderView : String -> Element Msg
listHeaderView listName =
    Element.row
        [ Background.color colors.purple
        , width fill
        , height shrink
        , Font.color colors.lime
        , Font.bold
        , Font.alignLeft
        ]
        [ backButton BackToListSelection
        , Element.el [ width (px 5) ] Element.none
        , textWithEllipsis listName
        , Element.el [ width (px 20) ] Element.none
        ]


shoppingListView : ShoppingList -> Element Msg
shoppingListView { name, completed, pending } =
    let
        sortedCompleted =
            List.sort completed

        sortedPending =
            List.sort pending

        backgroundColor state =
            case state of
                Pending ->
                    colors.grey

                Completed ->
                    colors.darkGrey

        itemToKeyedElement : ItemState -> Item -> ( String, Element Msg )
        itemToKeyedElement state item =
            ( item, itemView state item )

        listColumn state list =
            Keyed.column
                [ width fill
                , Background.color <| backgroundColor state
                , spacing 1
                ]
                (List.map (itemToKeyedElement state) list)
    in
    Element.column [ width fill, height fill ]
        [ listHeaderView name
        , Element.column
            [ height fill, width fill, spacing 1, Element.scrollbarX ]
            [ listColumn Pending sortedPending
            , listColumn Completed sortedCompleted
            , fabMargin
            ]
        ]


shoppingListPageView : ShoppingList -> Html Msg
shoppingListPageView shoppingList =
    Element.layout
        [ Background.color colors.black
        , floatingActionButton OpenItemCreator
        ]
        (shoppingListView shoppingList)
