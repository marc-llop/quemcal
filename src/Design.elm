module Design exposing (backButton, colors, fabMargin, floatingActionButton)

import Element exposing (Element, height, padding, px, rgb255, rgba255, width)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Icons
import Svg exposing (svg)
import Svg.Attributes as S


fabMargin =
    Element.el
        [ Element.height (Element.px 80)
        , Element.width Element.fill
        ]
        Element.none


fabSize : Int
fabSize =
    80


fabSizeStr =
    String.fromInt fabSize


plusCircleSvg : Html msg
plusCircleSvg =
    svg
        [ S.height fabSizeStr
        , S.width fabSizeStr
        , S.viewBox "0 0 60 60"
        , S.strokeLinecap "round"
        , S.strokeLinejoin "round"
        , S.strokeWidth "5"
        , S.stroke "currentColor"
        ]
        [ Svg.defs []
            [ Svg.mask [ S.id "cross" ]
                [ Svg.rect [ S.x "0", S.y "0", S.width "60", S.height "60", S.fill "white" ] []
                , Svg.line [ S.x1 "30", S.y1 "20", S.x2 "30", S.y2 "40", S.stroke "black" ] []
                , Svg.line [ S.x1 "20", S.y1 "30", S.x2 "40", S.y2 "30", S.stroke "black" ] []
                ]
            ]
        , Svg.circle [ S.cx "30", S.cy "30", S.r "25", S.mask "url(#cross)", S.fill "currentColor" ] []
        ]


floatingActionButton : msg -> Element.Attribute msg
floatingActionButton onPress =
    let
        fab =
            Element.el
                [ height (px fabSize)
                , width (px fabSize)
                , Font.color colors.lime
                ]
                (Element.html plusCircleSvg)
    in
    Input.button
        [ Element.alignRight
        , Element.alignBottom
        , padding 30
        , Element.focused []
        ]
        { onPress = Just onPress
        , label = fab
        }
        |> Element.inFront


backButton : msg -> Element msg
backButton message =
    let
        backIcon =
            Element.el
                [ width (px 64)
                , height (px 64)
                , Font.color colors.lime
                , Element.paddingXY 20 20
                ]
                (Element.html Icons.arrowLeft)
    in
    Input.button [ Element.focused [] ]
        { onPress = Just message
        , label = backIcon
        }


colors =
    { black = rgb255 6 2 6
    , backdropBlack = rgba255 6 2 6 0.8
    , darkGrey = rgb255 24 17 24
    , purple = rgb255 23 4 20
    , grey = rgb255 48 34 48
    , lime = rgb255 159 234 34
    , lightLime = rgb255 214 246 162
    }
