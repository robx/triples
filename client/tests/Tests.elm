module Tests exposing (..)

import Dict
import Expect
import Game
import Test exposing (..)


suite : Test
suite =
    let
        triplesGame =
            { deck = Game.deck, table = Dict.empty, type_ = Game.Triples, short = False }

        quadruplesGame =
            { triplesGame | type_ = Game.Quadruples }
    in
    describe "tests"
        [ test "grid has size 12" <|
            \() ->
                Expect.equal
                    (Game.standardGrid triplesGame |> List.length)
                    12
        , test "gaps finds all gaps in an empty table" <|
            \() ->
                Expect.equal
                    (triplesGame |> Game.gaps |> List.length)
                    12
        , test "gaps finds all gaps in an empty quadruples table" <|
            \() ->
                Expect.equal
                    (quadruplesGame |> Game.gaps |> List.length)
                    9
        , test "empty table is empty" <|
            \() ->
                Expect.equal
                    (Game.posEmpty triplesGame ( 1, 1 ))
                    True
        , test "full deck has 81 cards" <|
            \() ->
                Expect.equal
                    (List.length Game.deck)
                    81
        ]
