module Parse exposing (..)

import Date exposing (Date)
import Date.Extra exposing (utc, noTime, calendarDate)
import Date.Extra.Facts exposing (monthFromMonthNumber)
import Regex exposing (Regex, HowMany(All, AtMost))
import Dict exposing (Dict)


type alias Format =
    List Part


type Part
    = Pattern (Int -> String)
    | Literal String


{-|
TODO Support quarter
TODO Add long month
-}
dateRegex : Regex
dateRegex =
    let
        defaultGroups =
            String.join "|"
                [ "YYYY"
                , "YY"
                , "MM"
                , "M"
                , "DD"
                , "D"
                , "."
                ]
    in
        Regex.regex <| "(" ++ defaultGroups ++ ")"


type alias DateTuple =
    ( Int, Int, Int )


initialDateTuple : DateTuple
initialDateTuple =
    ( 1970, 1, 1 )


type DatePart
    = Year
    | Month
    | Day


someMap : Dict String ( DatePart, Int -> Int, String )
someMap =
    Dict.fromList
        [ ( "YYYY", ( Year, identity, "^(\\d{4}|\\d{2})(.*)" ) )
        ]



-- a) ]


parse : String -> String -> Result String Date
parse format input =
    let
        matches : List String
        matches =
            Regex.find All dateRegex format
                |> List.map .match
    in
        parseWithMatches matches input initialDateTuple


handleYear : DateTuple -> Int -> DateTuple
handleYear ( _, month, day ) y =
    let
        realY =
            if y <= 50 then
                y + 2000
            else if y <= 99 then
                y + 1900
            else
                y
    in
        ( realY, month, day )


parseWithMatches : List String -> String -> DateTuple -> Result String Date
parseWithMatches matches input (( year, month, day ) as tuple) =
    case matches of
        [] ->
            Ok <| tupleToDate tuple

        key :: xs ->
            let
                maybeDatePart =
                    Dict.get key someMap
            in
                case maybeDatePart of
                    Nothing ->
                        if String.startsWith key input then
                            parseWithMatches xs (String.dropLeft (String.length key) input) tuple
                        else
                            Err "Expected something else..."

                    Just ( datePart, f, regex ) ->
                        let
                            matches =
                                Regex.find (AtMost 1) (Regex.regex regex) input

                            subMatches =
                                matches
                                    |> List.head
                                    |> Maybe.map .submatches
                        in
                            case subMatches of
                                Just [ Just head, Just tail ] ->
                                    String.toInt head
                                        |> Result.andThen (\x -> patchTuple datePart (f x) tuple)

                                _ ->
                                    Err "Month matching failed"

        -- "MM" :: xs ->
        -- let
        --     matches =
        --         find (AtMost 1) (regex "^(\\d{1,2})(.*)") input
        -- in
        --     case matches |> List.head |> Maybe.map .submatches |> Maybe.withDefault [] of
        --         [ Just head, Just tail ] ->
        --             String.toInt head
        --                 |> Result.andThen (\m -> parseWithMatches xs tail ( year, m, day ))
        --
        --         _ ->
        --             Err "Month matching failed"
        _ :: xs ->
            Debug.log "B" <| Err "Not Implemented"

patchTuple : DatePart ->Int ->  DateTuple -> DateTuple
patchTuple datePart
doMatch : String -> (Int -> Result String DateTuple) -> String -> Result String DateTuple
doMatch regex f input =
    let
        matches =
            Regex.find (AtMost 1) (Regex.regex regex) input
    in
        case matches |> List.head |> Maybe.map .submatches |> Maybe.withDefault [] of
            [ Just head, Just tail ] ->
                String.toInt head
                    |> Result.andThen f

            _ ->
                Err "Year matching failed"


tupleToDate : DateTuple -> Date
tupleToDate ( y, d, m ) =
    Date.Extra.fromSpec
        utc
        noTime
        (calendarDate y (monthFromMonthNumber m) d)


createFormat : String -> Format
createFormat format =
    []
