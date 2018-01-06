module Game exposing (..)

import Card exposing (..)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List exposing (shuffle)


deckEmpty : Game -> Bool
deckEmpty g =
    List.isEmpty g.deck


posEmpty : Game -> Pos -> Bool
posEmpty g p =
    not <| Dict.member p g.table


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


initShort : Generator Game
initShort =
    shuffled
        |> Random.map (List.drop 60)
        |> Random.map
            (\d ->
                { deck = d
                , table = Dict.empty
                }
            )
        |> Random.map deal


over : Game -> Bool
over g =
    List.isEmpty g.deck && countSets g == 0


shuffled : Generator (List Card)
shuffled =
    shuffle deck


dealAction : Game -> Action
dealAction g =
    Deal (gaps g)


deal : Game -> Game
deal g =
    apply (dealAction g) g


dealMoreAction : Game -> Action
dealMoreAction =
    let
        col c =
            [ ( c, 0 ), ( c, 1 ), ( c, 2 ) ]
    in
    Deal << col << columns


dealMore : Game -> Game
dealMore g =
    apply (dealMoreAction g) g


compactMoves : Game -> List ( Pos, Pos )
compactMoves g =
    let
        f ( gap, pos ) =
            if gap < pos && pos > ( 3, 2 ) then
                Just ( pos, gap )
            else
                Nothing
    in
    List.filterMap f <|
        List.map2 (,) (allGaps g) (List.reverse <| Dict.keys <| g.table)


compact : Game -> ( Game, Pos -> Pos )
compact g =
    let
        ms =
            compactMoves g

        md =
            Dict.fromList ms

        move p =
            Maybe.withDefault p <| Dict.get p md
    in
    ( apply (Move ms) g, move )


type alias Game =
    { deck : List Card
    , table : Dict Pos Card
    }


type alias Pos =
    ( Int, Int )


type Action
    = Deal (List Pos)
    | Set (List Pos)
    | Move (List ( Pos, Pos ))


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

        move1 ( pos, gap ) g =
            let
                move from to dict =
                    case Dict.get from dict of
                        Nothing ->
                            dict

                        Just v ->
                            Dict.insert to v <| Dict.remove from <| dict
            in
            { g | table = move pos gap g.table }
    in
    case action of
        Deal ps ->
            List.foldr (<|) game (List.map deal1 ps)

        Set ps ->
            List.foldr (<|) game (List.map remove1 ps)

        Move ms ->
            List.foldr (<|) game (List.map move1 ms)


columns : Game -> Int
columns g =
    let
        last =
            List.reverse >> List.head
    in
    (\( x, y ) -> x + 1) <|
        Maybe.withDefault ( -1, -1 ) <|
            last <|
                Dict.keys <|
                    g.table


grid : Int -> List Pos
grid cols =
    List.range 0 (cols - 1) |> List.concatMap (\x -> List.range 0 2 |> List.map (\y -> ( x, y )))


standardGrid : List Pos
standardGrid =
    grid 4


gaps : Game -> List Pos
gaps g =
    List.filter (\p -> not <| Dict.member p g.table) standardGrid


allGaps : Game -> List Pos
allGaps g =
    List.filter (\p -> not <| Dict.member p g.table) (grid (columns g))


set : Game -> List Pos -> Bool
set g ps =
    let
        cs =
            List.filterMap (flip Dict.get g.table) ps
    in
    List.length ps == 3 && Card.set cs


take : Game -> List Pos -> ( Bool, Game )
take g ps =
    if set g ps then
        ( True, { g | table = List.foldr (<|) g.table (List.map Dict.remove ps) } )
    else
        ( False, g )


countSets : Game -> Int
countSets g =
    let
        cards =
            Dict.values g.table

        pairs xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    List.map (\z -> [ y, z ]) ys ++ pairs ys

        triples xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    (List.map ((::) y) <| pairs ys) ++ triples ys
    in
    List.length <| List.filter Card.set <| triples cards
