module Utils exposing (..)

-- Service modules

import Html exposing (Html, text)
import Date exposing (..)
import Task exposing (Task)
import List.Extra
import FormatNumber
import FormatNumber.Locales


-- Application modules

import Data.Shared exposing (DataID)


nothing : Html msg
nothing =
    text ""


lookupList : (a -> Bool) -> List a -> Maybe a
lookupList predicate list =
    case list of
        x :: xs ->
            if (predicate x) then
                Just x
            else
                lookupList predicate xs

        [] ->
            Nothing


createDateList : Date -> Date -> List Date
createDateList startDate endDate =
    let
        startTime =
            Date.toTime startDate

        endTime =
            Date.toTime endDate

        nextDay day =
            day + 86400000

        createList time1 time2 =
            if (time1 > time2) then
                []
            else
                Date.fromTime time1 :: createList (nextDay time1) time2
    in
        if (startTime < endTime) then
            createList startTime endTime
        else
            createList endTime startTime


findRecById : DataID a -> List ( DataID a, a ) -> Maybe ( DataID a, a )
findRecById id recordsList =
    List.Extra.find (\( recId, _ ) -> recId == id) recordsList


formatHours : Float -> String
formatHours =
    FormatNumber.format FormatNumber.Locales.usLocale



{- mapFilter {record}
   (((==) value) << .field)
   (.field)
   (\{innerRecord} innerValue -> { innerRecord | innerField = innerValue })
   (\innerField -> newInnerField)
-}


mapFilter : List a -> (a -> Bool) -> (a -> b) -> (a -> b -> a) -> (b -> b) -> List a
mapFilter list check getter setter transform =
    List.map
        (\a ->
            if (check a) then
                setter a <| transform <| getter a
            else
                a
        )
        list


roundToFirstSign : Float -> Float
roundToFirstSign value =
    let
        mul10 num =
            num * 10

        div10 num =
            num / 10
    in
        value
            |> mul10
            |> truncate
            |> toFloat
            |> div10


mapCurrentMonth : (Date -> Date -> Task x a) -> Date -> Task x a
mapCurrentMonth get now =
    let
        day =
            86400000

        dayInMonth =
            Date.day now

        currentTime =
            Date.toTime now

        daysInMonthVal =
            daysInMonth (Date.year now) (Date.month now)

        startMonth =
            Date.fromTime <| currentTime - (toFloat <| (dayInMonth - 1) * day)
    in
        get startMonth <| Date.fromTime <| (Date.toTime startMonth) + (toFloat <| (daysInMonthVal - 1) * day)


daysInMonth : Int -> Month -> Int
daysInMonth year month =
    case month of
        Jan ->
            31

        Feb ->
            if isLeapYear year then
                29
            else
                28

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    ((year % 4 == 0) && (year % 100 /= 0)) || ((year % 400) == 0)


equalDateNoTime : Date -> Date -> Bool
equalDateNoTime date1 date2 =
    (Date.year date1 == Date.year date2)
        && (Date.month date1 == Date.month date2)
        && (Date.day date1 == Date.day date2)


toHoursValue : Int -> String
toHoursValue amount =
    toString <| amount // 60


timeUnits =
    ( "hr", "Hours" )


prctUnits =
    ( "%", "%" )


(?) : Bool -> a -> a -> a
(?) conditional trueCase falseCase =
    if conditional then
        trueCase
    else
        falseCase
infixr 1 ?
