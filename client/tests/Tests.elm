module Tests exposing (..)

import Dict
import Expect
import Game
import Main
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
        , test "statistics work" <|
            \() ->
                Expect.equal
                    (Main.stat [ 2, 2, 1, 4, 4 ])
                    { average = 2.6, median = 2, min = 1, max = 4 }
        , test "statistics work, v 2" <|
            \() ->
                Expect.equal
                    (Main.stat [ 1, 2, 3, 4, 5 ])
                    { average = 3, median = 3, min = 1, max = 5 }
        , test "statistics work, v 3" <|
            \() ->
                Expect.equal
                    (Main.stat [ 1, 3, 4, 5 ])
                    { average = 3.25, median = 3.5, min = 1, max = 5 }
        ]
