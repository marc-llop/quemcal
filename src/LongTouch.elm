module LongTouch exposing (LongTouchModel, LongTouchMsg, initLongTouch, longTouchSubscription, shouldDeleteItem, updateLongTouch)

import ModelTypes exposing (Item)
import Time


type LongTouchMsg
    = TouchStart Item
    | TouchEnd
    | TouchingTick


type LongTouchState
    = Tachi Item
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


shouldDeleteItem : LongTouchMsg -> LongTouchModel -> Maybe Item
shouldDeleteItem msg model =
    case model.state of
        Tachi item ->
            if msg == TouchEnd && model.duration > minimumLongTouchDuration then
                Just item

            else
                Nothing

        NoTachi ->
            Nothing


longTouchSubscription : LongTouchModel -> Sub LongTouchMsg
longTouchSubscription { state } =
    case state of
        Tachi _ ->
            Time.every 100 (\_ -> TouchingTick)

        NoTachi ->
            Sub.none


updateLongTouch : LongTouchMsg -> LongTouchModel -> LongTouchModel
updateLongTouch msg model =
    case msg of
        TouchStart item ->
            { state = Tachi item, duration = 0 }

        TouchEnd ->
            { model | state = NoTachi }

        TouchingTick ->
            { model | duration = model.duration + refreshFrequency }
