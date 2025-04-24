module Hour exposing
    ( Time(..)
    , now, time, fromParts, fromPosix, fromIsoString, fromMillis, fromSeconds
    , hours, minutes, seconds, millis, parts
    , toIsoString, toMillis, toSeconds
    , Language, format, formatWithLanguage
    , Unit(..), add, diff
    , Interval(..), ceiling, floor, range
    , compare, isBetween, min, max, clamp
    , decoder, encode
    )

{-| This module provides a representation of local times in Elm.

It is called Hour to avoid confusion with elm/time module and to live in a
different namespace.

The API is based on justinmimbs/date, and both packages work well together to
represent both dates and times.


# Types

@docs Time


# Create

@docs now, time, fromParts, fromPosix, fromIsoString, fromMillis, fromSeconds


# Extract

@docs hours, minutes, seconds, millis, parts


# Conversions

@docs toIsoString, toMillis, toSeconds


# Formatting

@docs Language, format, formatWithLanguage


# Arithmetic

@docs Unit, add, diff


# Rounding and intervals

@docs Interval, ceiling, floor, range


# Ordering

@docs compare, isBetween, min, max, clamp


# Encoding

@docs decoder, encode

-}

import Json.Decode as D
import Json.Encode as E
import Pattern exposing (Token(..))
import Task exposing (Task)
import Time exposing (Posix, Zone)


{-| Represents a local time
-}
type Time
    = T
        { hours : Int
        , minutes : Int
        , seconds : Int
        , millis : Int
        }



---| CREATE


{-| Gets the current time in the local timezone.
-}
now : Task x Time
now =
    Time.here
        |> Task.andThen (\zone -> Time.now |> Task.map (fromPosix zone))


{-| Creates a new Time instance from a tuple of hours, minutes, and seconds.

Values are normalized to the range of 0-23 for hours, 0-59 for minutes, and
0-59 for seconds.

    time (13, 5, 0)
        |> toIsoString
    --> "13:05"

Out or range values are normalized:

    time (12, 60, 60)
        |> toIsoString
    --> "13:01"

-}
time : ( Int, Int, Int ) -> Time
time ( hh, mm, ss ) =
    fromParts { hours = hh, minutes = mm, seconds = ss, millis = 0 }


{-| Creates a new Time instance with milliseconds.

    fromParts { hours = 13, minutes = 5, seconds = 0 , millis = 1}
        |> toIsoString
    --> "13:05:00.001"

All values are normalized, so you can pass numbers in invalid ranges and they will
wrap up or down to correct values.

    fromParts {hours = -2, minutes = 100, seconds = 61, millis = 1500}
        |> parts
        --> {hours = 23, minutes = 41, seconds = 2, millis = 500}

-}
fromParts : { hours : Int, minutes : Int, seconds : Int, millis : Int } -> Time
fromParts data =
    (data.millis + 1000 * (data.seconds + 60 * (data.minutes + 60 * data.hours)))
        |> fromMillis


{-| Create a Time instance from a Posix timestamp.

    import Time

    fromPosix Time.utc (Time.millisToPosix (12 * 60 * 60 * 1000))
        |> toIsoString
    --> "12:00"

-}
fromPosix : Zone -> Posix -> Time
fromPosix zone posix =
    T
        { hours = Time.toHour zone posix
        , minutes = Time.toMinute zone posix
        , seconds = Time.toSecond zone posix
        , millis = Time.toMillis zone posix
        }


{-| Create a Time instance from duration from the start of the day in milliseconds.

    fromMillis (12 * 60 * 60 * 1000)
        |> toIsoString
    --> "12:00"

-}
fromMillis : Int -> Time
fromMillis ms =
    ms
        |> Time.millisToPosix
        |> fromPosix Time.utc


{-| Create a Time instance from duration from the start of the day in seconds.

    fromSeconds (12 * 60 * 60)
        |> toIsoString
    --> "12:00"

-}
fromSeconds : Int -> Time
fromSeconds sec =
    sec * 1000 |> fromMillis


{-| Parse iso8601 time string to Time.

Expect hours in the format "HH:MM:SS" or "HH:MM:SS.mmm".

    fromIsoString "12:30:05.500"
        |> Maybe.map parts
    --> Just { hours = 12, minutes = 30, seconds = 5, millis = 500 }

-}
fromIsoString : String -> Maybe Time
fromIsoString str =
    case String.split ":" str of
        [ hh, mm, ss ] ->
            case ( String.toInt hh, String.toInt mm, String.toFloat ss ) of
                ( Just h, Just m, Just s ) ->
                    Just
                        (T
                            { hours = h
                            , minutes = m
                            , seconds = Basics.floor s
                            , millis = Basics.floor (1000 * s) |> modBy 1000
                            }
                        )

                _ ->
                    Nothing

        _ ->
            Nothing



---| EXTRACT


{-| Extracts the hours from a Time

    time (12, 30, 5)
        |> hours
    --> 12

-}
hours : Time -> Int
hours (T data) =
    data.hours


{-| Extracts the minutes from a Time

    time (12, 30, 5)
            |> minutes
        --> 30

-}
minutes : Time -> Int
minutes (T data) =
    data.minutes


{-| Extracts the seconds from a Time

    time (12, 30, 5)
            |> seconds
        --> 5

-}
seconds : Time -> Int
seconds (T data) =
    data.seconds


{-| Extracts the milliseconds from a Time

    fromParts {hours = 12, minutes = 30, seconds = 5, millis = 500}
            |> millis
        --> 500

-}
millis : Time -> Int
millis (T data) =
    data.millis


{-| Extracts all components of a Time as a record

    fromParts {hours = 12, minutes = 30, seconds = 5, millis = 500}
            |> parts
        --> {hours = 12, minutes = 30, seconds = 5, millis = 500}

-}
parts : Time -> { hours : Int, minutes : Int, seconds : Int, millis : Int }
parts (T data) =
    data



---| CONVERSIONS


{-| Converts a Time to ISO 8601 strings.

Times are rendered as "HH:MM:SS.mmm", where the seconds and milliseconds parts
are ommited if possible.

    time (12, 30, 5)
        |> toIsoString
    --> "12:30:05"

    time (12, 30, 0)
        |> toIsoString
    --> "12:30"

-}
toIsoString : Time -> String
toIsoString value =
    let
        data =
            parts value
    in
    case ( data.seconds, data.millis ) of
        ( 0, 0 ) ->
            intDD data.hours ++ ":" ++ intDD data.minutes

        ( _, 0 ) ->
            intDD data.hours ++ ":" ++ intDD data.minutes ++ ":" ++ intDD data.seconds

        ( _, _ ) ->
            intDD data.hours
                ++ ":"
                ++ intDD data.minutes
                ++ ":"
                ++ intDD data.seconds
                ++ "."
                ++ intDDD data.millis


intD : Int -> String
intD =
    String.fromInt


intDD : Int -> String
intDD n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n


intDDD : Int -> String
intDDD n =
    if n < 100 then
        "0" ++ intDD n

    else
        String.fromInt n


{-| Convert time to milliseconds ellapsed from the start of the day

    fromParts {hours = 1, minutes = 0, seconds = 1, millis = 500}
        |> toMillis
    --> 3601500

-}
toMillis : Time -> Int
toMillis (T data) =
    data.millis + 1000 * (data.seconds + 60 * (data.minutes + 60 * data.hours))


{-| Convert time to seconds ellapsed from the start of the day

    fromParts {hours = 1, minutes = 0, seconds = 1, millis = 500}
        |> toSeconds
    --> 3601

-}
toSeconds : Time -> Int
toSeconds (T data) =
    data.seconds + 60 * (data.minutes + 60 * data.hours)



---| FORMATTING


type alias Language =
    { am : String
    , pm : String
    }


{-| Format a time using a string as a template.

    format "EEEE, d MMMM y" (fromOrdinalDate 1970 1)
        == "Thursday, 1 January 1970"

Alphabetic characters in the template represent time information; the number of
times a character is repeated specifies the form of a name or the padding of a number.

Alphabetic characters can be escaped within single-quotes; a single-quote can
be escaped as a sequence of two single-quotes, whether appearing inside or
outside an escaped sequence.

Templates are based on Date Format Patterns in [Unicode Technical Standard #35](https://www.unicode.org/reports/tr35/tr35-43/tr35-dates.html#Date_Format_Patterns).
Only the following subset of formatting characters are available:

    ```
    "h" -- The hour, using a 12-hour clock from 1 to 12.
    "hh" -- The hour, using a 12-hour clock from 01 to 12.
    "H" -- The hour, using a 24-hour clock from 0 to 23.
    "HH" -- The hour, using a 24-hour clock from 00 to 23.
    "m" -- The minute, from 0 through 59.
    "mm" -- The minute, from 00 through 59.
    "s" -- The second, from 0 through 59.
    "ss" -- The second, from 00 through 59.
    "a" -- The first character of the AM/PM designator.
    "aa" -- The AM/PM designator.
    "f" -- The tenths of a second in a date and time value.
    "ff" -- The hundredths of a second in a date and time value.
    "fff" -- The milliseconds in a date and time value.
    ```

    format "hh:mmaa 'and' s 'seconds'" (time (13, 15, 42))
        --> "01:15pm. and 42 seconds"

-}
format : String -> Time -> String
format =
    formatWithLanguage en


{-| Format a time using a string as a template, with a specific language.
-}
formatWithLanguage : Language -> String -> Time -> String
formatWithLanguage lang pattern =
    let
        tokens =
            pattern |> Pattern.fromString |> List.reverse
    in
    formatWithTokens lang tokens


formatWithTokens : Language -> List Token -> Time -> String
formatWithTokens language tokens data =
    List.foldl
        (\token formatted ->
            case token of
                Field char length ->
                    formatField language char length data ++ formatted

                Literal str ->
                    str ++ formatted
        )
        ""
        tokens


formatField : Language -> Char -> Int -> Time -> String
formatField lang char length (T data) =
    case ( char, length ) of
        ( 'h', 1 ) ->
            data.hours |> modBy 12 |> intD

        ( 'h', _ ) ->
            data.hours |> modBy 12 |> intDD

        ( 'H', 1 ) ->
            data.hours |> intD

        ( 'H', _ ) ->
            data.hours |> intDD

        ( 'm', 1 ) ->
            data.minutes |> intD

        ( 'm', _ ) ->
            data.minutes |> intDD

        ( 's', 1 ) ->
            data.seconds |> intD

        ( 's', _ ) ->
            data.seconds |> intDD

        ( 'a', 1 ) ->
            iff (data.hours < 12) lang.am lang.pm |> String.slice 0 1

        ( 'a', _ ) ->
            iff (data.hours < 12) lang.am lang.pm

        ( 'f', 1 ) ->
            "." ++ (data.millis // 100 |> intD)

        ( 'f', 2 ) ->
            "." ++ (data.millis // 10 |> intDD)

        ( 'f', _ ) ->
            "." ++ (data.millis |> intDDD)

        _ ->
            ""


en : Language
en =
    { am = "am."
    , pm = "pm."
    }


iff cond a b =
    if cond then
        a

    else
        b



---|  ARITHMETIC


type Unit
    = Hours
    | Minutes
    | Seconds
    | Milliseconds


{-| Get past or future time by adding a number of units to it.

    time ( 9, 0, 0 )
        |> add Hours 1
        |> toIsoString
    --> "10:00"

-}
add : Unit -> Int -> Time -> Time
add unit value time_ =
    let
        ms =
            toMillis time_
    in
    case unit of
        Hours ->
            ms
                + (value * 60 * 60 * 1000)
                |> fromMillis

        Minutes ->
            ms
                + (value * 60 * 1000)
                |> fromMillis

        Seconds ->
            ms
                + (value * 1000)
                |> fromMillis

        Milliseconds ->
            ms
                + value
                |> fromMillis


{-| Get the difference between two times, as a number of whole units.

    diff Minutes
        (time ( 10, 0, 0 ))
        (time ( 8, 0, 0 ))
        --> 120

-}
diff : Unit -> Time -> Time -> Int
diff unit (T final) (T initial) =
    let
        ( delta, sign ) =
            (toMillis (T final) - toMillis (T initial))
                |> splitSign
    in
    case unit of
        Hours ->
            (delta // (60 * 60 * 1000)) * sign

        Minutes ->
            (delta // (60 * 1000)) * sign

        Seconds ->
            (delta // 1000) * sign

        Milliseconds ->
            delta * sign


splitSign : Int -> ( Int, Int )
splitSign n =
    if n < 0 then
        ( -n, -1 )

    else
        ( n, 1 )



---|  ROUNDING AND INTERVALS


{-| Represent a time interval

```text
    Hour -- 1 hour
    Half -- 30 minutes
    Quarter -- 15 minutes
    Minute -- 1 minute
    Second -- 1 second
    Millisecond -- 1 millisecond
```

-}
type Interval
    = Hour
    | Half
    | Quarter
    | Minute
    | Second
    | Millisecond


{-| Round up a time to the beginning of the closest interval.

The resulting time will be greater than or equal to the one provided.

Any fractional time will be rounded up to the next whole interval.

    time ( 9, 0, 1 )
        |> Hour.ceiling Hour
        |> toIsoString
    --> "10:00"

But exact times will be unchanged:

    time ( 9, 0, 0 )
        |> Hour.ceiling Hour
        |> toIsoString
    --> "09:00"

-}
ceiling : Interval -> Time -> Time
ceiling interval value =
    let
        floored =
            value |> floor interval
    in
    if value == floored then
        value

    else
        let
            ( n, unit ) =
                interval |> intervalToUnits
        in
        floored |> add unit n


{-| Round down a time to the beginning of the closest interval.

The resulting time will be less than or equal to the one provided.

    time ( 9, 20, 1 )
        |> Hour.floor Quarter
        |> toIsoString
    --> "09:15"

-}
floor : Interval -> Time -> Time
floor interval ((T data) as value) =
    case interval of
        Hour ->
            T { data | minutes = 0, seconds = 0, millis = 0 }

        Half ->
            T { data | minutes = 30 * (data.minutes // 30), seconds = 0, millis = 0 }

        Quarter ->
            T { data | minutes = 15 * (data.minutes // 15), seconds = 0, millis = 0 }

        Minute ->
            T { data | seconds = 0, millis = 0 }

        Second ->
            T { data | millis = 0 }

        Millisecond ->
            value


{-| Create a list of times, at rounded intervals, increasing by a step value,
between two times.

The list will start on or after the first time, and end before the second time.

    range Half 3 (time ( 9, 0, 0 )) (time ( 17, 0, 0 ))
        |> List.map toIsoString
        --> [ "09:00", "10:30", "12:00", "13:30", "15:00", "16:30" ]

Notice the final time is not present in the list.

-}
range : Interval -> Int -> Time -> Time -> List Time
range interval step start end =
    let
        ( stepSize, unit ) =
            intervalToUnits interval

        timeList acc current =
            if compare current end == GT then
                List.reverse acc

            else
                timeList
                    (current :: acc)
                    (ceiling interval (add unit (step * stepSize) current))
    in
    timeList [] (ceiling interval start)


intervalToUnits : Interval -> ( Int, Unit )
intervalToUnits interval =
    case interval of
        Hour ->
            ( 1, Hours )

        Half ->
            ( 30, Minutes )

        Quarter ->
            ( 15, Minutes )

        Minute ->
            ( 1, Minutes )

        Second ->
            ( 1, Seconds )

        Millisecond ->
            ( 1, Milliseconds )



---|  ORDERING


{-| Compare two times. This can be used as the compare function for List.sortWith

    List.sortWith Hour.compare [ time ( 9, 0, 0 ), time ( 8, 0, 0 ) ]
        |> List.map toIsoString
    --> [ "08:00", "09:00" ]

-}
compare : Time -> Time -> Order
compare lhs rhs =
    Basics.compare (toMillis lhs) (toMillis rhs)


{-| Test if a time is within a range, inclusive, of the range values.

    time ( 8, 30, 0 )
        |> isBetween (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
    --> True

-}
isBetween : Time -> Time -> Time -> Bool
isBetween start end value =
    clamp start end value == value


{-| Get the smaller of two times.

    Hour.min (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
        |> toIsoString
    --> "08:00"

-}
min : Time -> Time -> Time
min lhs rhs =
    if toMillis lhs < toMillis rhs then
        lhs

    else
        rhs


{-| Get the larger of two times.

    Hour.max (time ( 8, 0, 0 )) (time ( 9, 0, 0 ))
        |> toIsoString
    --> "09:00"

-}
max : Time -> Time -> Time
max lhs rhs =
    if toMillis lhs > toMillis rhs then
        lhs

    else
        rhs


{-| Clamp a time within a range.

    clamp start end appointment
        (time ( 9, 0, 0 )) -- start
        (time ( 17, 0, 0 )) -- end
        (time ( 18, 0, 0 )) -- appointment
        |> toIsoString
    -- 17:00:00

-}
clamp : Time -> Time -> Time -> Time
clamp start end value =
    if compare start end == GT then
        clamp end start value

    else
        max start (min end value)



---| ENCODING


{-| JSON encoder for Time
-}
encode : Time -> E.Value
encode =
    toIsoString >> E.string


{-| JSON decoder for Time
-}
decoder : D.Decoder Time
decoder =
    D.string
        |> D.andThen
            (\str ->
                case fromIsoString str of
                    Just data ->
                        D.succeed data

                    Nothing ->
                        D.fail ("Invalid time format: " ++ str)
            )
