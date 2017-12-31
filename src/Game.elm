module Game exposing (..)

import Card exposing (..)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List exposing (shuffle)


deck : List Card
deck =
    List.map fromInt (List.range 0 80)


shuffled : Generator (List Card)
shuffled =
    shuffle deck


deal : List Card -> ( List Card, List Card )
deal cs =
    ( List.take 12 cs, List.drop 12 cs )


type alias Game =
    { deck : List Card
    , table : Dict Pos Card
    }


type alias Pos =
    ( Int, Int )


type Action
    = Deal (List Pos)
    | Set (List Pos)


apply : Action -> Game -> Game
apply action game =
    let
        deal1 pos g =
            case g.deck of
                d :: ds ->
                    { g | deck = ds, table = Dict.insert pos d g.table }

                [] ->
                    g

        remove1 pos g =
            { g | table = Dict.remove pos g.table }
    in
    case action of
        Deal ps ->
            List.foldr (<|) game (List.map deal1 ps)

        Set ps ->
            List.foldr (<|) game (List.map remove1 ps)