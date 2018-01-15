module Tests exposing (..)

import Dict
import Expect
import Game
import Test exposing (..)


suite : Test
suite =
    let
        setGame =
            { deck = Game.deck, table = Dict.empty, type_ = Game.ClassicSet, short = False }

        supersetGame =
            { setGame | type_ = Game.SuperSet }
    in
    describe "tests"
        [ test "grid has size 12" <|
            \() ->
                Expect.equal
                    (Game.standardGrid |> List.length)
                    12
        , test "gaps finds all gaps in an empty table" <|
            \() ->
                Expect.equal
                    (setGame |> Game.gaps |> List.length)
                    12
        , test "gaps finds all gaps in an empty superset table" <|
            \() ->
                Expect.equal
                    (supersetGame |> Game.gaps |> List.length)
                    9
        , test "empty table is empty" <|
            \() ->
                Expect.equal
                    (Game.posEmpty setGame ( 1, 1 ))
                    True
        , test "full deck has 81 cards" <|
            \() ->
                Expect.equal
                    (List.length Game.deck)
                    81
        ]
