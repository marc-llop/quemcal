module LongTouch exposing (LongTouchModel, LongTouchMsg(..), initLongTouch, longTouchSubscription, onLongTouch, shouldDeleteItem, touchEndInfo, updateLongTouch)

import Element exposing (Attribute)
import Html.Events
import Json.Decode as Decode
import Time


type LongTouchMsg data
    = TouchStart
    | TouchEnd data
    | TouchingTick


type LongTouchState
    = Tachi
    | NoTachi


type alias LongTouchModel =
    { state : LongTouchState
    , duration : Int
    }


{-| Refresh frequency for the timer, in milliseconds
-}
refreshFrequency : Int
refreshFrequency =
    100


{-| Minimum duration for a click to count as a long touch, in milliseconds
-}
minimumLongTouchDuration : Int
minimumLongTouchDuration =
    500


initLongTouch : LongTouchModel
initLongTouch =
    { state = NoTachi
    , duration = 0
    }


shouldDeleteItem : LongTouchMsg data -> LongTouchModel -> Bool
shouldDeleteItem msg model =
    case ( model.state, msg ) of
        ( Tachi, TouchEnd _ ) ->
            if model.duration > minimumLongTouchDuration then
                True

            else
                False

        ( _, _ ) ->
            False


touchEndInfo : LongTouchMsg data -> Maybe data
touchEndInfo msg =
    case msg of
        TouchEnd data ->
            Just data

        _ ->
            Nothing


longTouchSubscription : LongTouchModel -> Sub (LongTouchMsg data)
longTouchSubscription { state } =
    case state of
        Tachi ->
            Time.every 100 (\_ -> TouchingTick)

        NoTachi ->
            Sub.none


updateLongTouch : LongTouchMsg data -> LongTouchModel -> LongTouchModel
updateLongTouch msg model =
    case msg of
        TouchStart ->
            { state = Tachi, duration = 0 }

        TouchEnd _ ->
            { model | state = NoTachi }

        TouchingTick ->
            { model | duration = model.duration + refreshFrequency }


onTouchStart : (LongTouchMsg data -> msg) -> Attribute msg
onTouchStart msgWrapper =
    Html.Events.on "touchstart" (Decode.succeed <| msgWrapper TouchStart)
        |> Element.htmlAttribute


onTouchEnd : (LongTouchMsg data -> msg) -> data -> Attribute msg
onTouchEnd msgWrapper data =
    let
        msg =
            msgWrapper (TouchEnd data)
    in
    Html.Events.on "touchend" (Decode.succeed msg)
        |> Element.htmlAttribute


onLongTouch : (LongTouchMsg data -> msg) -> data -> List (Attribute msg)
onLongTouch msgWrapper data =
    [ onTouchStart msgWrapper
    , onTouchEnd msgWrapper data
    ]
