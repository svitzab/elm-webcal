module Model exposing (..)

import Animation exposing (..)
import Array exposing (..)
import Color exposing (Color, rgb)
import Date exposing (..)
import Mouse exposing (Position)
import Msgs exposing (..)
import Task exposing (..)

type alias Comm =
    {
        id: String
        , name: Maybe String
        , creator: Maybe String
        , genre: String
        , medium: String
        , start: Date
        , end: Date
        , price: Int
        , hours: Maybe Float
    }

type alias CommIds = 
    {
        ids: Array String
    }

ePos = {x = 0, y = 0}

type alias Dateable x =
    {x | start: Date, end: Date}

type alias TranslateOffsets =
    {
        months: Position
        , years: Position
    }

type alias ScrollInfo =
    {
    startDisplace: Position
    , shift: Int
    }

type alias Scrolls =
    {
    months: ScrollInfo
    , years: ScrollInfo
    }

eScrollInfo = (ScrollInfo ePos 0)

eScrolls = (Scrolls eScrollInfo eScrollInfo)

type alias CalendarStruct =
    {
        current: Date
        , selected: Array Date
        , focused: Date
        , broken: Bool
        , numWeeks: Int
        , shift: Int
        , wkdays: Array Day
    }

type alias Memo =
    {
        days: Array Date
        , values: Array Int
        , maxie: Int
        , diffs: Array (Int, Int)
        , borders: Array Int
        , colors: Array (Color, Color)
    }

type alias Model =
    {
        scrolls: Scrolls
        , offsets: TranslateOffsets
        , offsetting: Maybe String
        , dragStart: Maybe Position
        , calendar: CalendarStruct
        , memoized: Memo
    }

type alias AnimationClump =
    {
        interruptible: Bool
        , whole: Animation
    }

monthly_colors : Array (Color, Color)
monthly_colors = 
    Array.fromList [
    (rgb 249 164 164, rgb 255 124 124)
    , (rgb 168 204 255, rgb 124 179 255)
    , (rgb 164 252 215, rgb 124 255 200)
    , (rgb 207 170 255, rgb 181 124 255)
    , (rgb 218 255 173, rgb 196 255 124)
    ]

initialModel : Model
initialModel =
    Model eScrolls
        (TranslateOffsets ePos ePos)
        Nothing
        Nothing
        (CalendarStruct (Date.fromTime 0) empty (Date.fromTime 0)
            False
            15
            0
            (Array.fromList [
                Sun
                , Mon
                , Tue
                , Wed
                , Thu
                , Fri
                , Sat
            ])
            )
        (Memo empty empty 0 empty empty empty)