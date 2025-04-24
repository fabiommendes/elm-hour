
module Examples exposing (..)

{-| AUTO GENERATED -- DO NOT EDIT -}

{-|


These examples are only meant to give a feel for the library; see the docs for the full API.
Create a time and format it:

    import Hour

    Hour.time (13, 30, 0)
        |> Hour.format "hh'h and' mm 'minutes'"
        --> "01h and 30 minutes"

Basic arithmetic with hours:

    import Hour exposing (Interval(..), Unit(..))
    import Time exposing (Month(..))

    Hour.time (9, 23, 0)
        |> Hour.floor Quarter
        |> Hour.add Hours 8
        |> Hour.toIsoString
        == "17:15"

List all times in a 45 minutes interval for all working hours:

    import Hour exposing (Interval(..), Unit(..))

    Hour.range Minute 45 (Hour.time (9, 0, 0)) (Hour.time (16, 0, 0))
        |> List.map Hour.toIsoString
        --> ["09:00", "09:45", "10:30", "11:15", "12:00", "12:45", "13:30", "14:15", "15:00", "15:45"]

-}
test : Maybe Never
test =
    Nothing
