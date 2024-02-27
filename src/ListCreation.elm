module ListCreation exposing (listCreationPageView)

import Design exposing (colors)
import Dict exposing (values)
import Element exposing (Element, fill, height, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import ListSelection
import ModelTypes exposing (ShoppingList)
import Msg exposing (Msg(..))


backdrop : Element msg -> Element msg
backdrop child =
    Element.el
        [ width fill
        , height fill
        , Background.color colors.backdropBlack
        ]
        child


modal : Element msg -> Element msg
modal child =
    Element.el
        [ width shrink
        , height shrink
        , Border.rounded 10
        , Background.color colors.darkGrey
        , Font.color colors.lime
        , Element.centerX
        , Element.centerY
        ]
        child


textInput : String -> Element Msg
textInput value =
    Input.text
        [ Background.color colors.grey
        , Border.rounded 22
        , Border.width 0
        , Element.paddingXY 20 12
        , height (px 44)
        , Element.focused []
        , Font.alignLeft
        , Font.color colors.lightLime
        , Font.glow colors.lime 1.0
        ]
        { onChange = UpdateEditedList
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.alignLeft, Element.moveUp 10 ] <| Element.text "Doneu nom a la llista:"
        }


listCreationForm : String -> Element Msg
listCreationForm editedList =
    Element.column
        [ Element.spacing 20

        -- , Element.explain Debug.todo
        ]
        [ Element.el
            [ Element.padding 40 ]
            (textInput editedList)
        , Element.row
            [ Background.color colors.grey
            , Border.roundEach
                { topLeft = 0
                , topRight = 0
                , bottomLeft = 10
                , bottomRight = 10
                }
            , width fill
            , Element.clipX
            , height (px 50)
            ]
            [ Input.button
                []
                { onPress = Just BackToListSelection
                , label = Element.text "Cancel·la"
                }
            , Input.button
                []
                { onPress = Just (AddList editedList)
                , label = Element.text "Afegeix"
                }
            ]
        ]


listCreationView : String -> Element Msg
listCreationView editedList =
    backdrop <|
        modal <|
            listCreationForm editedList


listCreationPageView : List ShoppingList -> String -> Html Msg
listCreationPageView lists editedList =
    Element.layout
        [ Background.color colors.black
        , Element.inFront (listCreationView editedList)
        ]
        (ListSelection.listSelectionView lists)
