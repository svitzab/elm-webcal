module Utilities exposing (..)

import Debug exposing (..)

import Array exposing (..)
import Color exposing (..)
import Color.Interpolate exposing (..)
import Date exposing (..)
import Date.Extra.Core exposing (monthToInt)
import Date.Extra.Compare exposing (..)
import Date.Extra.Duration as Dura
import Maybe exposing (withDefault)
import Model exposing (Dateable, monthly_colors)

dda : Maybe Date -> Date
dda n =
    case n of
        Just x ->
            x
        Nothing ->
            fromTime 0

x_get : Int -> (Maybe a -> a) -> Array a -> a
x_get ind func arr =
    if ind == -1 || ind >= Array.length arr then
        func <| get (Array.length arr - 1) arr
    else
        func <| get ind arr

wkdayInt : Date -> Int
wkdayInt date =
    let
        dayy = dayOfWeek date
    in
        case dayy of
            Sun ->
                0
            Mon ->
                1
            Tue ->
                2
            Wed ->
                3
            Thu ->
                4
            Fri ->
                5
            Sat ->
                6

wkdayString : Day -> String
wkdayString date =
    case date of
        Sun ->
            "Sun"
        Mon ->
            "Mon"
        Tue ->
            "Tue"
        Wed ->
            "Wed"
        Thu ->
            "Thu"
        Fri ->
            "Fri"
        Sat ->
            "Sat"

monthToString : Date -> String
monthToString date =
    let
        monthh = month date
    in
        case monthh of
            Jan ->
                "January"
            Feb ->
                "February"
            Mar ->
                "March"
            Apr ->
                "April"
            May ->
                "May"
            Jun ->
                "June"
            Jul ->
                "July"
            Aug ->
                "August"
            Sep ->
                "September"
            Oct ->
                "October"
            Nov ->
                "November"
            Dec ->
                "December"

addDay = Dura.add Dura.Day

addWeek = Dura.add Dura.Week

addMonth = Dura.add Dura.Month

sameDay : Date -> Date -> Bool
sameDay first second =
    day first == day second && month first == month second
        && year first == year second

sameMonth : Date -> Date -> Bool
sameMonth first second =
    month first == month second && year first == year second

sameYear : Date -> Date -> Bool
sameYear first second =
    let
        rec = Dura.diff first second
    in
        rec.year == 0

daysArray : Date -> Date -> Array Date -> Array Date
daysArray start end days =
    let
        next = addDay 1 start
    in
        if sameDay start end then
            Array.push start days
        else
            daysArray next end (Array.push start days)

monthsArray : Date -> Date -> Array Date -> Array Date
monthsArray start end months =
    let
        next = addMonth 1 start
    in
        if sameMonth start end then
            Array.push start months
        else
            monthsArray next end (Array.push start months)

colToStr : Color -> String
colToStr color =
    let
        col = toRgb color
    in
        "rgb(" ++ (toString col.red) ++ "," ++ (toString col.blue) ++ "," ++ (toString col.green) ++ ")"

strPolate : Color -> Color -> Float -> String
strPolate min max val =
    colToStr (interpolate RGB min max val)

overlapDayInd : Array Date -> Array Date -> Array Int
overlapDayInd first second =
    if is After (x_get 0 dda first) (x_get -1 dda second)
        || is Before (x_get -1 dda first) (x_get 0 dda second) then
        empty
    else
        let
            (smaller, start, end) = if Array.length first < Array.length second then
                        (Array.toIndexedList first, x_get 0 dda second, x_get -1 dda second)
                    else
                        (Array.toIndexedList second, x_get 0 dda first, x_get -1 dda first)
        in
            List.foldl (\(ind, a) b -> if is3 BetweenOpen a start end then
                                    Array.push ind b
                                else
                                    b) empty smaller

overlappingDays : Array Date -> Array Date -> Array Date
overlappingDays first second =
    if is After (x_get 0 dda first) (x_get -1 dda second)
        || is Before (x_get -1 dda first) (x_get 0 dda second) then
        empty
    else
        let
            (smaller, start, end) = if Array.length first < Array.length second then
                        (first, x_get 0 dda second, x_get -1 dda second)
                    else
                        (second, x_get 0 dda first, x_get -1 dda first)
        in
            Array.foldl (\a b -> if is3 BetweenOpen a start end then
                                    Array.push a b
                                else
                                    b) empty smaller

objsPerDay : (Date, Date) -> Array Date -> Array Int -> Array Int
objsPerDay (objStart, objEnd) days numObjs =
    let
        o = overlapDayInd (daysArray objStart objEnd empty) days
    in
        if Array.length o < 1 then
            numObjs
        else
            Array.foldl (\a b -> let
                                    prev = withDefault 0 <| get a b
                                 in
                                    set a (prev+1) b) numObjs o

genericScrape : Array (Dateable x) -> Array (Date, Date)
genericScrape objs =
    Array.foldl (\a b -> Array.push (a.start, a.end) b) empty objs

accumulateDays : Array (Date, Date) -> Array Date -> Array Int
accumulateDays days cal =
    let
        initial = Array.repeat (Array.length cal) 0
    in
        Array.foldl (\a b -> objsPerDay a cal initial) initial days

datesIn : (Date, Date) -> Array Date -> Int
datesIn (start, end) potentials =
    Array.foldl (\a b -> if is3 BetweenOpenEnd a start end then
                            b + 1
                        else
                            b) 0 potentials

weekStartRange : Int -> Int -> Date -> (Date, Date)
weekStartRange weeks shift point =
    let
        (sstart, eend) = if weeks % 2 == 1 then
            (addWeek (negate <| weeks // 2) point, addWeek (weeks // 2) point)
                        else if weeks > 0 then
            (addWeek (negate <| (weeks // 2) - 1) point, addWeek ((weeks // 2)) point)
                        else
            (point, point)
        (start, end) = (addWeek shift sstart, addWeek shift eend)
        p_beg = wkdayInt start
        p_end = wkdayInt end
    in
        (addDay -p_beg start, addDay (6 - p_end) end)

buildBorders : Array Date -> Date -> Array Int
buildBorders days current =
    Array.map (\a -> 
        let
            borders = if (not <| sameMonth (addDay 1 a) a)
                        && dayOfWeek a /= Sat then
                    2
                        else if not <| sameMonth (addWeek 1 a) a then
                    1
                        else
                    0
        in
            if not <| sameDay a current then
                borders
            else
                if borders > 0 then
                    negate borders
                else
                    -3) days

buildColors : Array Date -> Array (Color, Color)
buildColors days =
    Array.map (\a ->    let
            mappedMonth = monthToInt <| month a
                        in
            withDefault (black, black)
            <| get (mappedMonth % (Array.length monthly_colors))
            monthly_colors) days