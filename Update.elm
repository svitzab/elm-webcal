module Update exposing (..)

import Debug exposing (..)

import Array exposing (..)
import Date exposing (..)
import Maybe as M
import Model exposing (..)
import Mouse exposing (Position)
import Msgs exposing (..)
import Result as R
import String exposing (endsWith)
import Utilities exposing (..)

h = 50
ddetect = 9

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)
        Getnow date ->
            let
                cal = model.calendar
                newcal = {cal | current = date, focused = date}
                newmemo = buildMemo (genericScrape empty) newcal
            in
                ({model | calendar = newcal
                    , memoized = newmemo}, Cmd.none)
        MouseDrop {className, position} ->
            let
                off = if endsWith "day" className || 
                        endsWith "month" className then
                        Just className
                    else
                        Nothing
                pos = case off of
                        Just _ ->
                            Just position
                        Nothing ->
                            Nothing
            in 
                ({model | offsetting = off, dragStart = pos}, Cmd.none)
        DragAt newPoint ->
        case model.offsetting of
            Nothing ->
                (model, Cmd.none)
            Just x ->
            case x of
                "webcal-month" ->
                    let
                        diffie = 
                            case model.dragStart of
                                Nothing ->
                                    newPoint
                                Just point ->
                                    point
                        monthinfo = model.scrolls.months
                        (nMonthinfo, nMonthdiff) =
                            infiniScroll diffie newPoint h monthinfo
                        moffs = model.offsets
                        mscos = model.scrolls
                        cal = model.calendar
                        (newoffs, newscrolls, newcal) =
                            ({moffs | months =
                                {x = 0, y = nMonthdiff}}
                            , {mscos | months = nMonthinfo}
                            , {cal | shift = nMonthinfo.shift})
                        newmemo = if nMonthinfo.shift == cal.shift then
                                    model.memoized
                                else
                                    (buildPartialMemo (genericScrape empty) 
                                    cal newcal model.memoized)
                    in
                        ({model | offsets = newoffs, 
                        scrolls = newscrolls
                        , calendar = newcal
                        , memoized = newmemo}, Cmd.none)
                _ ->
                    (model, Cmd.none)
        DragEnd endPoint ->
            let
                mscos = model.scrolls
                offset = M.withDefault "" model.offsetting
                sReset = 
                        {mscos | months =
                        (ScrollInfo ePos mscos.months.shift)}
                safe = {model | offsetting = Nothing
                        , dragStart = Nothing
                        , scrolls = sReset}
            in
                case model.dragStart of
                    Nothing ->
                        (safe, Cmd.none)
                    Just point ->
                        if abs (endPoint.y - point.y) < ddetect then
                            (safe, Cmd.none)
                        else
                    (safe, Cmd.none)

infiniScroll : Position -> Position -> Int -> ScrollInfo -> (ScrollInfo, Int)
infiniScroll s np factor info =
    let
        (sx, sy) = (s.x, s.y)
        (npx, npy) = (np.x, np.y)
        diff = {x = sx, y = info.startDisplace.y}
        ydiff = if abs (npy - sy) < ddetect then
                    0
                else
                    npy - (diff.y + sy)
        (nstart, nshift, rdiff) = if ydiff >= factor-10 then
                ({diff | y = diff.y + factor}
                , info.shift - 1
                , ydiff - factor)
                    else if ydiff <= negate (factor-10) then
                ({diff | y = diff.y - factor}
                , info.shift + 1
                , ydiff + factor)
                    else
                (diff, info.shift, ydiff)
    in
        (ScrollInfo nstart nshift, rdiff)

buildPartialMemo : Array (Date, Date) -> CalendarStruct -> CalendarStruct -> Memo -> Memo
buildPartialMemo dateRanges cal desired memo =
    let
        shift = desired.shift
        chunk = abs <| shift - cal.shift
        directshift = if shift > cal.shift then
                        (cal.numWeeks // 2 + 2)
                    else
                        negate <| (cal.numWeeks // 2 + 2)
        newshift = if cal.numWeeks % 2 == 0
                    && directshift > 0 then
                        cal.shift + directshift - 1
                    else
                        cal.shift + directshift
        tempcal = {cal | shift = newshift
                        , numWeeks = (chunk-3)}
        partmemo = buildMemo dateRanges tempcal
        end = Array.length memo.days
        start = Array.length partmemo.days
    in
        (Memo
            (iac memo.days partmemo.days directshift start end)
            (iac memo.values partmemo.values directshift start end)
            (if partmemo.maxie > memo.maxie then
                    partmemo.maxie
                else
                    memo.maxie)
            empty
            (iac memo.borders partmemo.borders directshift start end)
            (iac memo.colors partmemo.colors directshift start end)
        )

iac : Array a -> Array a -> Int -> Int -> Int -> Array a
iac first second compare newer older =
    if compare > 0 then
        let
            sliced = slice newer older first
        in
            Array.append sliced second
    else
        let
            sliced = slice 0 (negate newer) first
        in
            Array.append second sliced

buildMemo : Array (Date, Date) -> CalendarStruct -> Memo
buildMemo dateRanges cal =
    let
        (spreadStart, spreadEnd) = weekStartRange (cal.numWeeks + 2)
                (cal.shift) cal.focused
        spread = daysArray spreadStart spreadEnd empty
        values = accumulateDays dateRanges spread
        maxie = Array.foldl (\a b -> if a > b then
                                        a
                                    else
                                        b) 0 values
        wkdayMems = Array.toList cal.wkdays
        filtered = Array.filter (\a -> List.member (dayOfWeek a) wkdayMems) spread
        {-
        startsies = Array.map (\(a, b) -> a) dateRanges
        endsies = Array.map (\(a, b) -> b) dateRanges
        diffs = Array.foldl (\a b ->
                if a == 0 then
                    let
                        filtHead = (x_get 0 dda filtered, x_get 0 dda filtered)
                        newval = (datesIn filtHead startsies, datesIn filtHead endsies)
                    in
                        set a newval b
                else if a == (Array.length filtered) - 1 then
                    let
                        filtEnd = (x_get -1 dda filtered, x_get -1 dda filtered)
                        newval = (datesIn filtEnd startsies, datesIn filtEnd endsies)
                    in
                        set a newval b
                else
                    let
                        d = (x_get (a-1) dda filtered, x_get a dda filtered)
                        newval = (datesIn d startsies, datesIn d endsies)
                    in
                        set a newval b
                    )
            (Array.repeat (Array.length filtered) (0, 0))
            (initialize (Array.length filtered) identity)
            -}
        bords = buildBorders filtered cal.current
        cols = buildColors filtered
    in
        (Memo filtered values maxie empty bords cols)