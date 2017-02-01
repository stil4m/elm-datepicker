module Tests exposing (..)

import Date
import Expect
import Parse exposing (parse)
import Test exposing (..)


all : Test
all =
    describe "Parse"
        [ test "parse year" <|
            \() ->
                Parse.parse "YYYY" "2016"
                    |> Expect.equal (Date.fromString "2016-01-01")
        , test "parse year" <|
            \() ->
                Parse.parse "dd-mm-YYYY" "16"
                    |> Expect.equal (Date.fromString "0016-01-01")
        , test "parse month" <|
            \() ->
                Parse.parse "YYYY" "12"
                    |> Expect.equal (Date.fromString "2016-01-01")
        ]
