module Main exposing (main)

import Browser
import Element exposing (Element, fill, height, px, rgb255, spacing, text, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Icons
import Msg exposing (Msg(..))


main : Program () Int Msg
main =
    Browser.sandbox { init = 0, update = update, view = view }


update : Msg -> number -> number
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view : Int -> Html Msg
view model =
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
            [ listHeaderView
            , Element.column
                [ height fill, width fill, spacing 1, Element.scrollbarX ]
                [ listColumn Pending
                    [ itemView { state = Pending, text = "Potatoes" }
                    , itemView { state = Pending, text = "Cucumbers" }
                    , itemView { state = Pending, text = "Bananas" }
                    , itemView { state = Pending, text = "Tomatoes" }
                    , itemView { state = Pending, text = "Onions" }
                    , itemView { state = Pending, text = "Carrots" }
                    , itemView { state = Pending, text = "Spinachs" }
                    ]
                , listColumn Completed
                    [ itemView { state = Completed, text = "Cockles" }
                    , itemView { state = Completed, text = "Tuna" }
                    , itemView { state = Completed, text = "Salmon" }
                    , itemView { state = Completed, text = "Haddock" }
                    , itemView { state = Completed, text = "Cod" }
                    ]
                ]
            ]
        )


listHeaderView =
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
        , Background.color colors.grey
        , width fill
        ]
        [ headerIcon
        , Element.el
            [ Font.color colors.lightLime
            , Font.bold
            ]
            (text "List title")
        ]


type ItemState
    = Completed
    | Pending


type alias Item =
    { state : ItemState, text : String }


itemText item =
    let
        color =
            case item.state of
                Completed ->
                    [ Font.color colors.lime ]

                Pending ->
                    [ Font.color colors.lightLime, Font.glow colors.lime 2.0 ]
    in
    Element.el
        (width fill :: color)
        (text item.text)


itemCheckbox item =
    let
        color =
            case item.state of
                Completed ->
                    colors.lime

                Pending ->
                    colors.lightLime

        icon =
            case item.state of
                Completed ->
                    Icons.checkSquare

                Pending ->
                    Icons.square
    in
    Element.el
        [ width (px 32)
        , Font.color color
        , Font.glow colors.lime 2.0
        ]
        (Element.html icon)


itemView item =
    let
        backgroundColor =
            case item.state of
                Completed ->
                    colors.black

                Pending ->
                    colors.grey
    in
    Element.row
        [ width fill
        , Element.padding 20
        , Background.color backgroundColor
        , Element.spacing 15
        ]
        [ itemCheckbox item
        , Element.el [] <| itemText item
        ]


colors =
    { black = rgb255 6 2 6
    , grey = rgb255 24 17 24
    , lightGrey = rgb255 48 34 48
    , lime = rgb255 159 234 34
    , lightLime = rgb255 214 246 162
    }
