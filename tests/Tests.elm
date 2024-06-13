module Tests exposing (..)

import Date exposing (..)
import Expect
import Main exposing (..)
import Test exposing (..)
import Time exposing (..)



-- Check out https://package.elm-lang.org/packages/elm-explorations/test/latest to learn more about testing in Elm!


all : Test
all =
    describe "something"
        [ test "1st of June" <|
            \_ ->
                Expect.equal
                    (takesTimeandReturnsRow (fromCalendarDate 2024 Time.Jun 1))
                    1
        , test "today baby" <|
            \_ ->
                Expect.equal
                    (takesTimeandReturnsRow (fromCalendarDate 2024 Time.Jun 6))
                    2
        , test "it's been.. one week" <|
            \_ ->
                Expect.equal
                    (takesTimeandReturnsRow (fromCalendarDate 2024 Time.Jun 13))
                    3
        , test "august 1 2025" <|
            \_ ->
                Expect.equal
                    (takesTimeandReturnsRow (fromCalendarDate 2024 Time.Aug 1))
                    1
        ]
