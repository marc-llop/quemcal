module Design exposing (colors, fabMargin)

import Element exposing (rgb255)


fabMargin =
    Element.el
        [ Element.height (Element.px 70)
        , Element.width Element.fill
        ]
        Element.none


colors =
    { black = rgb255 6 2 6
    , grey = rgb255 24 17 24
    , purple = rgb255 23 4 20
    , lightGrey = rgb255 48 34 48
    , lime = rgb255 159 234 34
    , lightLime = rgb255 214 246 162
    }
