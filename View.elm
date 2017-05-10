module View exposing (..)

import Debug exposing (..)

import Array exposing (..)
import Color exposing (..)
import Date exposing (..)
import Date.Extra.Core exposing (toFirstOfMonth)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (..)
import Json.Decode as Json
import Maybe exposing (withDefault)
import Model exposing (..)
import Mouse exposing (Position)
import Msgs exposing (..)
import Utilities exposing (..)

he = 50

view : Model -> Html Msg
view model =
    if model.calendar.current == Date.fromTime 0 
        || model.calendar.numWeeks < 1 then
        div [] []
    else
        div [style [
                ("width", "80%")
                , ("margin-left", "10%")
                , ("-webkit-user-select", "none")
                , ("-moz-user-select", "none")
                ]
                , onMouseDowns MouseDrop
                , class "wc-blank"]
                [
                div [style [
                    ("width", "84%")
                    , ("margin-left", "16%")
                    , ("height", "80px")
                    , ("cursor", "move")
                    ]] 
                    <| composeWkdays model
                , div [style [
                    ("width", "100%")
                    , ("height", toPx (model.calendar.numWeeks * he))
                    , ("text-align", "center")
                    , ("overflow", "hidden")
                    ]]
                    [
                    div [style [
                        ("width", "16%")
                        , ("display", "inline-block")
                        , ("vertical-align", "top")
                        , ("position", "relative")
                        , ("font-size", "2em")
                        , ("cursor", "move")
                        ]
                        ]
                        [div [style [
                            ("height", toPx ((model.calendar.numWeeks + 2) * he))
                            , ("position", "relative")
                            , ("top", toPx (negate he))
                            , ("transform", posToTranslate model.offsets.months)
                            ]
                            , class "webcal-month"]
                        <| composeMonths model
                        ]
                    , div [style [
                        ("width", "84%")
                        , ("display", "inline-block")
                        , ("position", "relative")
                        , ("vertical-align", "top")
                        , ("font-size", "1.2em")
                        , ("line-height", toPx (he // 2))
                        , ("cursor", "pointer")
                        ]
                        , class "webcal-calendar"]
                        [div [style [
                            ("height", toPx ((model.calendar.numWeeks + 2) * he))
                            , ("position", "absolute")
                            , ("top", toPx (negate he))
                            , ("transform", posToTranslate model.offsets.months)
                        ]]
                        <| composeDays model
                        ]
                    ]
                ]

composeMonths : Model -> List (Html Msg)
composeMonths model =
    let
        (s, e) = weekStartRange (model.calendar.numWeeks + 2)
                (model.scrolls.months.shift) model.calendar.focused
        months = monthsArray s e empty
        prelocations = List.foldl (\a b ->
            let
                checkweek = addWeek a s
                recentmonth = x_get (Array.length b) dda months
            in
                if sameMonth recentmonth checkweek 
                && Array.length b < Array.length months then
                    push a b
                else
                    b
                ) empty (List.range 0 <| model.calendar.numWeeks+2)
        entry = 1
        gap = 2
        scrolled = model.offsets.months.y
        (zero, one) = (x_get 0 (withDefault 0) prelocations
                        , x_get 1 (withDefault 0) prelocations)
        diff = one - entry
        emp = List.repeat (Array.length months) []
        (locations, adjust) = if gap * he < diff * he + scrolled then
                        (Array.toList <| set 0 (entry) prelocations
                            , [("transform", posToTranslate 
                                (Position 0 <| negate scrolled))]
                                :: (List.drop 1 emp))
                    else if entry * he >= one * he + scrolled then
                        (Array.toList <| set 0 (one - gap) prelocations
                            , [] ::
                                [("transform", posToTranslate 
                                (Position 0 <| negate scrolled))]
                                :: (List.drop 2 emp))
                    else
                        (Array.toList <| set 0 (one - gap) prelocations
                            , emp)
    in
        List.map (\(mon, index, floaty) -> 
            div [style <| (++) [
                ("width", "100%")
                , ("position", "absolute")
                , ("top", toPx (index * he))
                ]
                <| floaty
                , class "webcal-month"]
                [
                div [style [
                    ("width", "1px")
                    , ("word-wrap", "break-word")
                    ]
                    , class "webcal-month"] 
                    [text <| String.toUpper <| String.left 3 (monthToString mon)]
                ]
            )
            (List.map3 (,,) (Array.toList months) locations adjust)

composeWkdays : Model -> List (Html Msg)
composeWkdays model =
    let
        wid = 100 / (toFloat <| Array.length model.calendar.wkdays)
    in
        Array.toList <| Array.map (\a -> div [style [
                ("display", "inline-block")
                , ("position", "relative")
                , ("vertical-align", "top")
                , ("width", toString wid ++ "%")
                , ("height", "30px")
                ]
                , class "webcal-wkday"]
                [div [style [
                    ("line-height", "30px")
                    ]] 
                    [text <| wkdayString a]]) model.calendar.wkdays

composeDays : Model-> List (Html Msg)
composeDays model =
    let
        wid = 100 / (toFloat <| Array.length model.calendar.wkdays)
        combined = initialize (Array.length model.memoized.days) identity
    in
        Array.toList <| Array.map (\a -> 
            let
                bord = withDefault 0 <| get a model.memoized.borders
                (sCol, eCol) = withDefault (black, black)
                    <| get a model.memoized.colors
            in
                div [style ([
            ("display", "inline-block")
            , ("position", "relative")
            , ("width", toString wid ++ "%")
            , ("height", "50px")
            , ("vertical-align", "top")
            , ("overflow", "visible")
            , if bord < 0 then
                ("background", "#ccc")
            else
                ("background", "#fff")
            ]
            ++ 
                {-if abs bord == 2 then
                [("box-shadow", "inset 0px -1px 0 0 black,"
                    ++ " inset -1px 0 0 0 black")]
                else if abs bord == 1 then
                [("box-shadow", "inset 0px -1px 0 0 black")]
                else
                    -}
                    []
            )] [div [style [
                    ("display", "inline-block")
                    , ("position", "relative")
                    , ("width", "80%")
                    , ("height", "100%")
                    , ("margin-x", "10%")
                    {-
                    , ("box-shadow", 
                        let
                            prev = "inset 0px -3px 0px 0px " ++ (colToStr eCol)
                                ++ ", inset 0px " ++ (toPx <| negate <| he//2) 
                                ++ " 0px 0px " ++ (colToStr sCol)
                            bb = if bord > 0 then
                            "inset 0px -1px 0px 0px #000, "
                            else
                                ""
                        in
                            bb ++ prev
                        )
                        -}
                    ]
                    , class "webcal-day"] 
                    [text <| toString <| day 
                    (withDefault (Date.fromTime 0) <| get a model.memoized.days)]
                ]) combined

toPx : Int -> String
toPx input =
    (toString input) ++ "px"

posToTranslate : Position -> String
posToTranslate {x, y} =
    "translate(" ++ (toString x) ++ "px, " ++ (toString y) ++ "px)"

{-
decodeWidth : Json.Decoder String
decodeWidth =
    Json.map String (Json.at ["target", "offsetWidth"] Json.string)
    -}

decodeId : Json.Decoder ClassMouse
decodeId =
    Json.map2 ClassMouse
        (Json.at ["target", "className"] Json.string)
        (Mouse.position)

onMouseDowns : (ClassMouse -> msg) -> Attribute msg
onMouseDowns tagger =
  on "mousedown" (Json.map tagger decodeId)

{-
onLoad : (String -> msg) -> Attribute msg
onLoad tagger =
    on "load" (Json.map tagger decodeWidth)
    -}