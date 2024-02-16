module Design exposing (colors, fabMargin, floatingActionButton)

import Element exposing (height, padding, px, rgb255, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Icons


fabMargin =
    Element.el
        [ Element.height (Element.px 80)
        , Element.width Element.fill
        ]
        Element.none


floatingActionButton : msg -> Element.Attribute msg
floatingActionButton onPress =
    let
        outerGlow =
            Element.el
                [ height (px 60)
                , width (px 60)
                , Border.solid
                , Border.width 2
                , Border.rounded 30
                , Border.glow colors.lime 2.0
                ]
                Element.none

        innerGlow =
            Element.el
                [ height (px 60)
                , width (px 60)
                , Border.solid
                , Border.width 2
                , Border.color colors.lightLime
                , Border.rounded 30
                , Border.innerGlow colors.lime 1.0
                ]
                Element.none

        plusIcon =
            Element.el
                [ Font.color colors.lightLime
                , Element.centerX
                , Element.centerY

                -- , width (px 30)
                -- , height (px 30)
                ]
                (Element.html Icons.plus)

        fab =
            Element.el
                [ height (px 60)
                , width (px 60)
                , Element.behindContent outerGlow
                , Element.behindContent innerGlow
                ]
                plusIcon

        fabButton =
            Input.button
                []
                { onPress = Just onPress
                , label = fab
                }
    in
    Element.el
        [ Element.alignRight
        , Element.alignBottom
        , padding 30
        ]
        fabButton
        |> Element.inFront


colors =
    { black = rgb255 6 2 6
    , grey = rgb255 24 17 24
    , purple = rgb255 23 4 20
    , lightGrey = rgb255 48 34 48
    , lime = rgb255 159 234 34
    , lightLime = rgb255 214 246 162
    }
