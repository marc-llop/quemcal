module ListSelection exposing (listSelectionPageView, listSelectionView)

import Design exposing (colors, fabMargin, floatingActionButton)
import Element exposing (Element, fill, fillPortion, height, padding, paddingEach, paragraph, px, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html exposing (Html)
import Msg exposing (Msg(..))
import ShoppingList exposing (ShoppingList)


progressBarView : Int -> Element msg
progressBarView progressPct =
    Element.row
        [ Border.rounded 3
        , width fill
        , height (px 6)
        , Background.color colors.grey
        ]
        [ Element.el
            [ Border.rounded 3
            , width (fillPortion progressPct)
            , height (px 6)
            , Background.color colors.lime
            ]
            Element.none
        , Element.el
            [ width (fillPortion (100 - progressPct)) ]
            Element.none
        ]


listThumbnailView : ShoppingList -> Element Msg
listThumbnailView { name, completed, pending } =
    let
        completedAmount =
            List.length completed

        totalAmount =
            completedAmount + List.length pending

        progressPct =
            (completedAmount * 100) // totalAmount

        progressText =
            String.fromInt completedAmount ++ " / " ++ String.fromInt totalAmount
    in
    Element.column
        [ Border.rounded 10
        , Background.color colors.purple
        , padding 30
        , spacing 5
        , width fill
        , Font.color colors.lightLime
        , Font.glow colors.lime 2.0
        , Font.alignLeft
        ]
        [ Element.row [ width fill, spacing 20 ]
            [ paragraph
                [ Element.alignLeft
                , width fill
                , paddingEach { left = 0, right = 0, top = 0, bottom = 10 }
                ]
                [ text name ]
            , paragraph
                [ Element.alignRight
                , Element.alignBottom
                , width shrink
                , Font.size 16
                ]
                [ text progressText ]
            ]
        , progressBarView progressPct
        ]


listThumbnailButtonView : ShoppingList -> Element Msg
listThumbnailButtonView shoppingList =
    Input.button
        [ width fill, Element.focused [] ]
        { onPress = Just (SelectList shoppingList.name)
        , label = listThumbnailView shoppingList
        }


listSelectionView : List ShoppingList -> Element Msg
listSelectionView lists =
    let
        sortedLists =
            List.sortBy .name lists

        listToKeyedThumbnail : ShoppingList -> ( String, Element Msg )
        listToKeyedThumbnail l =
            ( l.name, listThumbnailButtonView l )
    in
    Element.column
        [ Background.color colors.black
        , height fill
        ]
        [ Keyed.column
            [ Element.scrollbarY
            , width fill
            , height fill
            , spacing 20
            , padding 40
            ]
            (List.map listToKeyedThumbnail sortedLists
                ++ [ ( "---fabMargin---", fabMargin ) ]
            )
        ]


listSelectionPageView : List ShoppingList -> Html Msg
listSelectionPageView lists =
    Element.layout
        [ Background.color colors.black
        , floatingActionButton OpenListCreator
        ]
        (listSelectionView lists)
