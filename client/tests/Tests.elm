module Tests exposing (..)

import Dict
import Expect
import Game
import Test exposing (..)


suite : Test
suite =
    describe "tests"
        [ test "grid has size 12" <|
            \() ->
                Expect.equal
                    (Game.standardGrid |> List.length)
                    12
        , test "gaps finds all gaps in an empty table" <|
            \() ->
                Expect.equal
                    ({ deck = Game.deck, table = Dict.empty } |> Game.gaps |> List.length)
                    12
        , test "empty table is empty" <|
            \() ->
                Expect.equal
                    (Game.posEmpty { deck = Game.deck, table = Dict.empty } ( 1, 1 ))
                    True
        , test "full deck has 81 cards" <|
            \() ->
                Expect.equal
                    (List.length Game.deck)
                    81
        ]
