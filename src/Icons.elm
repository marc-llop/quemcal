module Icons exposing
    ( arrowLeft
    , check
    , checkSquare
    , edit2
    , list
    , plus
    , search
    , square
    , trash2
    )

import Html exposing (Html)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


svgFeatherIcon : String -> List (Svg msg) -> Html msg
svgFeatherIcon className =
    svg
        [ class <| "feather feather-" ++ className
        , fill "none"
        , height "24"
        , stroke "currentColor"
        , strokeLinecap "round"
        , strokeLinejoin "round"
        , strokeWidth "2"
        , viewBox "0 0 24 24"
        , width "24"
        ]


arrowLeft : Html msg
arrowLeft =
    svgFeatherIcon "arrow-left"
        [ Svg.line [ x1 "19", y1 "12", x2 "5", y2 "12" ] []
        , Svg.polyline [ points "12 19 5 12 12 5" ] []
        ]


checkSquare : Html msg
checkSquare =
    svgFeatherIcon "check-square"
        [ Svg.polyline [ points "9 11 12 14 22 4" ] []
        , Svg.path [ d "M21 12v7a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h11" ] []
        ]


edit2 : Html msg
edit2 =
    svgFeatherIcon "edit-2"
        [ Svg.path [ d "M17 3a2.828 2.828 0 1 1 4 4L7.5 20.5 2 22l1.5-5.5L17 3z" ] []
        ]


plus : Html msg
plus =
    svgFeatherIcon "plus"
        [ Svg.line [ x1 "12", y1 "5", x2 "12", y2 "19" ] []
        , Svg.line [ x1 "5", y1 "12", x2 "19", y2 "12" ] []
        ]


square : Html msg
square =
    svgFeatherIcon "square"
        [ Svg.rect [ Svg.Attributes.x "3", y "3", width "18", height "18", rx "2", ry "2" ] []
        ]


trash2 : Html msg
trash2 =
    svgFeatherIcon "trash-2"
        [ Svg.polyline [ points "3 6 5 6 21 6" ] []
        , Svg.path [ d "M19 6v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6m3 0V4a2 2 0 0 1 2-2h4a2 2 0 0 1 2 2v2" ] []
        , Svg.line [ x1 "10", y1 "11", x2 "10", y2 "17" ] []
        , Svg.line [ x1 "14", y1 "11", x2 "14", y2 "17" ] []
        ]


search : Html msg
search =
    svgFeatherIcon "search"
        [ Svg.circle [ cx "11", cy "11", r "8" ] []
        , Svg.line [ x1 "21", y1 "21", x2 "16.65", y2 "16.65" ] []
        ]


check : Html msg
check =
    svgFeatherIcon "check"
        [ Svg.polyline [ points "20 6 9 17 4 12" ] []
        ]


list : Html msg
list =
    svgFeatherIcon "list"
        [ Svg.line [ x1 "8", y1 "6", x2 "21", y2 "6" ] []
        , Svg.line [ x1 "8", y1 "12", x2 "21", y2 "12" ] []
        , Svg.line [ x1 "8", y1 "18", x2 "21", y2 "18" ] []
        , Svg.line [ x1 "3", y1 "6", x2 "3.01", y2 "6" ] []
        , Svg.line [ x1 "3", y1 "12", x2 "3.01", y2 "12" ] []
        , Svg.line [ x1 "3", y1 "18", x2 "3.01", y2 "18" ] []
        ]
