module ListCreation exposing (listCreationPageView, listNameInputId)

import Design exposing (colors)
import Dict exposing (values)
import Element exposing (Attribute, Element, fill, fillPortion, height, px, shrink, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import ListSelection
import Model.ShoppingList exposing (ShoppingList)
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


listNameInputId : String
listNameInputId =
    "list-name-input"


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
        , Element.htmlAttribute (Html.Attributes.id listNameInputId)
        ]
        { onChange = UpdateEditedList
        , text = value
        , placeholder = Nothing
        , label = Input.labelAbove [ height (px 40), Font.alignLeft, Element.centerY ] <| Element.text "Doneu nom a la llista:"
        }


type ActionButton
    = LeftCancel
    | RightAccept


actionButton :
    ActionButton
    ->
        { onPress : Maybe msg
        , label : Element msg
        }
    -> Element msg
actionButton side =
    let
        ( bottomLeft, bottomRight, color ) =
            case side of
                LeftCancel ->
                    ( 10, 0, colors.red )

                RightAccept ->
                    ( 0, 10, colors.lime )
    in
    Input.button
        [ width (fillPortion 1)
        , height fill
        , Font.color color
        , Border.color colors.grey
        , Border.solid
        , Border.width 1
        , Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = bottomLeft
            , bottomRight = bottomRight
            }
        ]


buttonsRow : String -> Element Msg
buttonsRow editedList =
    Element.row
        [ Border.roundEach
            { topLeft = 0
            , topRight = 0
            , bottomLeft = 10
            , bottomRight = 10
            }
        , width fill
        , Element.clipX
        , height (px 50)
        ]
        [ actionButton LeftCancel
            { onPress = Just BackToListSelection
            , label = Element.text "Cancel·la"
            }
        , actionButton RightAccept
            { onPress = Just (AddList editedList)
            , label = Element.text "Afegeix"
            }
        ]


listCreationForm : String -> Element Msg
listCreationForm editedList =
    Element.column
        []
        [ Element.el
            [ Element.padding 40 ]
            (textInput editedList)
        , buttonsRow editedList
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
