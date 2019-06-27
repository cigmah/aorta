module Types.Datetime exposing
    ( monthToString
    , posixToMonth
    , posixToString
    )

import Json.Decode as Decode
import Time exposing (Month(..), Posix, millisToPosix, posixToMillis)


posixToString : Posix -> String
posixToString time =
    let
        aedt =
            11 * 60

        aest =
            { start = 25909380, offset = 10 * 60 }

        zone =
            Time.customZone aedt [ aest ]

        year =
            Time.toYear zone time

        month =
            Time.toMonth zone time

        day =
            String.padLeft 2 '0' <| String.fromInt <| Time.toDay zone time

        hour =
            String.padLeft 2 '0' <| String.fromInt <| Time.toHour zone time

        minute =
            String.padLeft 2 '0' <| String.fromInt <| Time.toMinute zone time
    in
    monthToString month ++ " " ++ day ++ " " ++ hour ++ ":" ++ minute


posixToMonth : Posix -> String
posixToMonth time =
    let
        zone =
            Time.customZone (11 * 60) []

        month =
            Time.toMonth zone time
    in
    monthToString month


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "Jan"

        Feb ->
            "Feb"

        Mar ->
            "Mar"

        Apr ->
            "Apr"

        May ->
            "May"

        Jun ->
            "Jun"

        Jul ->
            "Jul"

        Aug ->
            "Aug"

        Sep ->
            "Sep"

        Oct ->
            "Oct"

        Nov ->
            "Nov"

        Dec ->
            "Dec"
