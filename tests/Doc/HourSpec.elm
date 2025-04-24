module Doc.HourSpec exposing (spec)

import Test
import Expect
import Hour exposing(..)
import Time


spec : Test.Test
spec =
    Test.describe "Hour" <|
        [ Test.describe "#time" <|
            [ Test.test "Example: 1 -- `time (13, 5, 0) |> toIsoString --> \"1...`" <|
                \() ->
                    Expect.equal
                        (
                            time (13, 5, 0)
                                |> toIsoString
                        )
                        (
                            "13:05"
                        )
            , Test.test "Example: 2 -- `time (12, 60, 60) |> toIsoString --> ...`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 60, 60)
                                |> toIsoString
                        )
                        (
                            "13:01"
                        )
            ]
        , Test.describe "#fromParts : { hours : Int, minutes : Int, seconds : Int, millis" <|
            [ Test.test "Example: 1 -- `fromParts { hours = 13, minutes = 5, ...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts { hours = 13, minutes = 5, seconds = 0 , millis = 1}
                                |> toIsoString
                        )
                        (
                            "13:05:00.001"
                        )
            , Test.test "Example: 2 -- `fromParts {hours = -2, minutes = 100,...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts {hours = -2, minutes = 100, seconds = 61, millis = 1500}
                                |> parts
                        )
                        (
                            {hours = 23, minutes = 41, seconds = 2, millis = 500}
                        )
            ]
        , Test.describe "#fromPosix" <|
            [ Test.test "Example: 1 -- `fromPosix Time.utc (Time.millisToPosi...`" <|
                \() ->
                    Expect.equal
                        (
                            fromPosix Time.utc (Time.millisToPosix (12 * 60 * 60 * 1000))
                                |> toIsoString
                        )
                        (
                            "12:00"
                        )
            ]
        , Test.describe "#fromMillis" <|
            [ Test.test "Example: 1 -- `fromMillis (12 * 60 * 60 * 1000) |> t...`" <|
                \() ->
                    Expect.equal
                        (
                            fromMillis (12 * 60 * 60 * 1000)
                                |> toIsoString
                        )
                        (
                            "12:00"
                        )
            ]
        , Test.describe "#fromSeconds" <|
            [ Test.test "Example: 1 -- `fromSeconds (12 * 60 * 60) |> toIsoSt...`" <|
                \() ->
                    Expect.equal
                        (
                            fromSeconds (12 * 60 * 60)
                                |> toIsoString
                        )
                        (
                            "12:00"
                        )
            ]
        , Test.describe "#fromIsoString" <|
            [ Test.test "Example: 1 -- `fromIsoString \"12:30:05.500\" |> Maybe...`" <|
                \() ->
                    Expect.equal
                        (
                            fromIsoString "12:30:05.500"
                                |> Maybe.map parts
                        )
                        (
                            Just { hours = 12, minutes = 30, seconds = 5, millis = 500 }
                        )
            ]
        , Test.describe "#hours" <|
            [ Test.test "Example: 1 -- `time (12, 30, 5) |> hours --> 12`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 30, 5)
                                |> hours
                        )
                        (
                            12
                        )
            ]
        , Test.describe "#minutes" <|
            [ Test.test "Example: 1 -- `time (12, 30, 5) |> minutes --> 30`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 30, 5)
                                    |> minutes
                        )
                        (
                            30
                        )
            ]
        , Test.describe "#seconds" <|
            [ Test.test "Example: 1 -- `time (12, 30, 5) |> seconds --> 5`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 30, 5)
                                    |> seconds
                        )
                        (
                            5
                        )
            ]
        , Test.describe "#millis" <|
            [ Test.test "Example: 1 -- `fromParts {hours = 12, minutes = 30, ...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts {hours = 12, minutes = 30, seconds = 5, millis = 500}
                                    |> millis
                        )
                        (
                            500
                        )
            ]
        , Test.describe "#parts : Time -> { hours : Int, minutes : Int, seconds : Int, millis" <|
            [ Test.test "Example: 1 -- `fromParts {hours = 12, minutes = 30, ...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts {hours = 12, minutes = 30, seconds = 5, millis = 500}
                                    |> parts
                        )
                        (
                            {hours = 12, minutes = 30, seconds = 5, millis = 500}
                        )
            ]
        , Test.describe "#toIsoString" <|
            [ Test.test "Example: 1 -- `time (12, 30, 5) |> toIsoString --> \"...`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 30, 5)
                                |> toIsoString
                        )
                        (
                            "12:30:05"
                        )
            , Test.test "Example: 2 -- `time (12, 30, 0) |> toIsoString --> \"...`" <|
                \() ->
                    Expect.equal
                        (
                            time (12, 30, 0)
                                |> toIsoString
                        )
                        (
                            "12:30"
                        )
            ]
        , Test.describe "#toMillis" <|
            [ Test.test "Example: 1 -- `fromParts {hours = 1, minutes = 0, se...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts {hours = 1, minutes = 0, seconds = 1, millis = 500}
                                |> toMillis
                        )
                        (
                            3601500
                        )
            ]
        , Test.describe "#toSeconds" <|
            [ Test.test "Example: 1 -- `fromParts {hours = 1, minutes = 0, se...`" <|
                \() ->
                    Expect.equal
                        (
                            fromParts {hours = 1, minutes = 0, seconds = 1, millis = 500}
                                |> toSeconds
                        )
                        (
                            3601
                        )
            ]
        , Test.describe "#format" <|
            [ Test.test "Example: 1 -- `format \"hh:mmaa 'and' s 'seconds'\" (t...`" <|
                \() ->
                    Expect.equal
                        (
                            format "hh:mmaa 'and' s 'seconds'" (time (13, 15, 42))
                        )
                        (
                            "01:15pm. and 42 seconds"
                        )
            ]
        , Test.describe "#add" <|
            [ Test.test "Example: 1 -- `time ( 9, 0, 0 ) |> add Hours 1 |> to...`" <|
                \() ->
                    Expect.equal
                        (
                            time ( 9, 0, 0 )
                                |> add Hours 1
                                |> toIsoString
                        )
                        (
                            "10:00"
                        )
            ]
        , Test.describe "#diff" <|
            [ Test.test "Example: 1 -- `diff Minutes (time ( 10, 0, 0 )) (tim...`" <|
                \() ->
                    Expect.equal
                        (
                            diff Minutes
                                (time ( 10, 0, 0 ))
                                (time ( 8, 0, 0 ))
                        )
                        (
                            120
                        )
            ]
        , Test.describe "#ceiling" <|
            [ Test.test "Example: 1 -- `time ( 9, 0, 1 ) |> Hour.ceiling Hour...`" <|
                \() ->
                    Expect.equal
                        (
                            time ( 9, 0, 1 )
                                |> Hour.ceiling Hour
                                |> toIsoString
                        )
                        (
                            "10:00"
                        )
            , Test.test "Example: 2 -- `time ( 9, 0, 0 ) |> Hour.ceiling Hour...`" <|
                \() ->
                    Expect.equal
                        (
                            time ( 9, 0, 0 )
                                |> Hour.ceiling Hour
                                |> toIsoString
                        )
                        (
                            "09:00"
                        )
            ]
        , Test.describe "#floor" <|
            [ Test.test "Example: 1 -- `time ( 9, 20, 1 ) |> Hour.floor Quart...`" <|
                \() ->
                    Expect.equal
                        (
                            time ( 9, 20, 1 )
                                |> Hour.floor Quarter
                                |> toIsoString
                        )
                        (
                            "09:15"
                        )
            ]
        , Test.describe "#range" <|
            [ Test.test "Example: 1 -- `range Half 3 (time ( 9, 0, 0 )) (time...`" <|
                \() ->
                    Expect.equal
                        (
                            range Half 3 (time ( 9, 0, 0 )) (time ( 17, 0, 0 ))
                                |> List.map toIsoString
                        )
                        (
                            [ "09:00", "10:30", "12:00", "13:30", "15:00", "16:30" ]
                        )
            ]
        , Test.describe "#compare" <|
            [ Test.test "Example: 1 -- `List.sortWith Hour.compare [ time ( 9...`" <|
                \() ->
                    Expect.equal
                        (
                            List.sortWith Hour.compare [ time ( 9, 0, 0 ), time ( 8, 0, 0 ) ]
                                |> List.map toIsoString
                        )
                        (
                            [ "08:00", "09:00" ]
                        )
            ]
        , Test.describe "#isBetween" <|
            [ Test.test "Example: 1 -- `time ( 8, 30, 0 ) |> isBetween (time ...`" <|
                \() ->
                    Expect.equal
                        (
                            time ( 8, 30, 0 )
                                |> isBetween (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
                        )
                        (
                            True
                        )
            ]
        , Test.describe "#min" <|
            [ Test.test "Example: 1 -- `Hour.min (time ( 8, 0, 0 )) (time ( 9...`" <|
                \() ->
                    Expect.equal
                        (
                            Hour.min (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
                                |> toIsoString
                        )
                        (
                            "08:00"
                        )
            ]
        , Test.describe "#max" <|
            [ Test.test "Example: 1 -- `Hour.max (time ( 8, 0, 0 )) (time ( 9...`" <|
                \() ->
                    Expect.equal
                        (
                            Hour.max (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
                                |> toIsoString
                        )
                        (
                            "09:00"
                        )
            ]
    ]