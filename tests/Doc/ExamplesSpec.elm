module Doc.ExamplesSpec exposing (spec)

import Test
import Expect
import Examples exposing(..)
import Hour
import Hour exposing (Interval(..), Unit(..))
import Time exposing (Month(..))
import Hour exposing (Interval(..), Unit(..))


spec : Test.Test
spec =
    Test.describe "Examples" <|
        [ Test.describe "#test" <|
            [ Test.test "Example: 1 -- `Hour.time (13, 30, 0) |> Hour.format ...`" <|
                \() ->
                    Expect.equal
                        (
                            Hour.time (13, 30, 0)
                                |> Hour.format "hh'h and' mm 'minutes'"
                        )
                        (
                            "01h and 30 minutes"
                        )
            , Test.test "Example: 2 -- `Hour.range Minute 45 (Hour.time (9, 0...`" <|
                \() ->
                    Expect.equal
                        (
                            Hour.range Minute 45 (Hour.time (9, 0, 0)) (Hour.time (16, 0, 0))
                                |> List.map Hour.toIsoString
                        )
                        (
                            ["09:00", "09:45", "10:30", "11:15", "12:00", "12:45", "13:30", "14:15", "15:00", "15:45"]
                        )
            ]
    ]