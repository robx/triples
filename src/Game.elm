module Game exposing (..)

import Card exposing (..)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List exposing (shuffle)


deck : List Card
deck =
    List.map fromInt (List.range 0 80)


none : Game
none =
    { deck = [], table = Dict.empty }


init : Generator Game
init =
    shuffled
        |> Random.map
            (\d ->
                { deck = d
                , table = Dict.empty
                }
            )
        |> Random.map deal


shuffled : Generator (List Card)
shuffled =
    shuffle deck


dealAction : Game -> Action
dealAction g =
    Deal (gaps g)


deal : Game -> Game
deal g =
    apply (dealAction g) g


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


grid : List Pos
grid =
    List.range 0 2 |> List.concatMap (\r -> List.range 0 3 |> List.map (\c -> ( r, c )))


gaps : Game -> List Pos
gaps g =
    List.filter (\p -> not <| Dict.member p g.table) grid
