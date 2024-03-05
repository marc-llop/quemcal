module LongTouch exposing (LongTouchModel, LongTouchMsg(..), initLongTouch, longTouchSubscription, shouldDeleteItem, updateLongTouch)

import Time


type LongTouchMsg data
    = TouchStart data
    | TouchEnd
    | TouchingTick


type LongTouchState data
    = Tachi data
    | NoTachi


type alias LongTouchModel data =
    { state : LongTouchState data
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


initLongTouch : LongTouchModel data
initLongTouch =
    { state = NoTachi
    , duration = 0
    }


shouldDeleteItem : LongTouchMsg data -> LongTouchModel data -> Maybe data
shouldDeleteItem msg model =
    case model.state of
        Tachi data ->
            if msg == TouchEnd && model.duration > minimumLongTouchDuration then
                Just data

            else
                Nothing

        NoTachi ->
            Nothing


longTouchSubscription : LongTouchModel data -> Sub (LongTouchMsg data)
longTouchSubscription { state } =
    case state of
        Tachi _ ->
            Time.every 100 (\_ -> TouchingTick)

        NoTachi ->
            Sub.none


updateLongTouch : LongTouchMsg data -> LongTouchModel data -> LongTouchModel data
updateLongTouch msg model =
    case msg of
        TouchStart item ->
            { state = Tachi item, duration = 0 }

        TouchEnd ->
            { model | state = NoTachi }

        TouchingTick ->
            { model | duration = model.duration + refreshFrequency }
