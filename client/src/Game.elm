module Game exposing (..)

import Card exposing (..)
import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List exposing (shuffle)


type alias Game =
    { deck : List Card
    , table : Dict Pos Card
    , type_ : GameType
    , short : Bool
    }


type alias GameView =
    { mincols : Int
    , rows : Int
    , table : Dict Pos Card
    , deckSize : Int
    , matchSize : Int
    }


rows : Int
rows =
    3


toView : Game -> GameView
toView g =
    { mincols = defaultColumns g.type_
    , rows = rows
    , table = g.table
    , deckSize = List.length g.deck
    , matchSize = gameMatchSize g
    }


type alias Pos =
    ( Int, Int )


type GameType
    = Triples
    | Quadruples


type alias GameDef =
    { type_ : GameType
    , short : Bool
    , multi : Bool
    }


defTriples =
    { type_ = Triples, short = False, multi = False }


defQuadruples =
    { type_ = Quadruples, short = False, multi = False }


defTriplesMulti =
    { type_ = Triples, short = False, multi = True }


deckEmpty : Game -> Bool
deckEmpty g =
    List.isEmpty g.deck


posEmpty : Game -> Pos -> Bool
posEmpty g p =
    viewPosEmpty (toView g) p


viewPosEmpty : GameView -> Pos -> Bool
viewPosEmpty g p =
    not <| Dict.member p g.table


defaultColumns : GameType -> Int
defaultColumns t =
    case t of
        Triples ->
            4

        Quadruples ->
            3


matchSize : GameType -> Int
matchSize t =
    case t of
        Triples ->
            3

        Quadruples ->
            4


gameMatchSize : Game -> Int
gameMatchSize g =
    matchSize g.type_


deck : List Card
deck =
    List.map fromInt (List.range 0 80)


empty : GameDef -> GameView
empty def =
    { deckSize = 81
    , table = Dict.empty
    , mincols = defaultColumns def.type_
    , rows = rows
    , matchSize = matchSize def.type_
    }


init : GameDef -> Generator Game
init def =
    shuffled
        |> Random.map
            (if def.short then
                List.drop 60
             else
                \x -> x
            )
        |> Random.map
            (\d ->
                { deck = d
                , table = Dict.empty
                , type_ = def.type_
                , short = def.short
                }
            )
        |> Random.map deal


over : Game -> Bool
over g =
    List.isEmpty g.deck && count g == 0


shuffled : Generator (List Card)
shuffled =
    shuffle deck


dealToAction : Game -> List Pos -> Action
dealToAction g ps =
    Deal <| List.map2 (,) ps g.deck


dealAction : Game -> Action
dealAction g =
    dealToAction g (gaps g)


deal : Game -> Game
deal g =
    apply (dealAction g) g


dealMoreAction : Game -> Action
dealMoreAction g =
    let
        col c =
            List.range 0 (rows - 1) |> List.map (\r -> ( c, r ))
    in
    dealToAction g <| col <| columns g


dealMore : Game -> Game
dealMore g =
    apply (dealMoreAction g) g


compactMoves : Game -> List ( Pos, Pos )
compactMoves g =
    let
        f ( gap, pos ) =
            if gap < pos && pos >= ( defaultColumns g.type_, 0 ) then
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


type Action
    = Deal (List ( Pos, Card ))
    | Match (List Pos)
    | Move (List ( Pos, Pos ))


apply : Action -> Game -> Game
apply action game =
    let
        deal1 ( pos, c ) g =
            { g | deck = List.drop 1 g.deck, table = Dict.insert pos c g.table }

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

        Match ps ->
            List.foldr (<|) game (List.map remove1 ps)

        Move ms ->
            List.foldr (<|) game (List.map move1 ms)


viewApply : Action -> GameView -> GameView
viewApply action game =
    let
        deal1 ( pos, c ) g =
            { g | deckSize = g.deckSize - 1, table = Dict.insert pos c g.table }

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

        Match ps ->
            List.foldr (<|) game (List.map remove1 ps)

        Move ms ->
            List.foldr (<|) game (List.map move1 ms)


selectedApply : Action -> List Pos -> List Pos
selectedApply action selected =
    let
        remove1 pos =
            List.filter ((/=) pos)

        move1 ( from, to ) =
            List.map <|
                \pos ->
                    if pos == from then
                        to
                    else
                        pos
    in
    case action of
        Deal ps ->
            selected

        Match ps ->
            if List.any (\s -> List.member s ps) selected then
                []
            else
                selected

        Move ms ->
            List.foldr (<|) selected (List.map move1 ms)


viewColumns : GameView -> Int
viewColumns g =
    let
        last =
            List.reverse >> List.head

        maxcol =
            (\( x, y ) -> x + 1) <|
                Maybe.withDefault ( -1, -1 ) <|
                    last <|
                        Dict.keys <|
                            g.table
    in
    max maxcol g.mincols


columns : Game -> Int
columns g =
    viewColumns <| toView g


grid : Int -> List Pos
grid cols =
    List.range 0 (cols - 1) |> List.concatMap (\x -> List.range 0 2 |> List.map (\y -> ( x, y )))


standardGrid : Game -> List Pos
standardGrid g =
    grid (defaultColumns g.type_)


gaps : Game -> List Pos
gaps g =
    List.filter (\p -> not <| Dict.member p g.table) <|
        standardGrid g


allGaps : Game -> List Pos
allGaps g =
    List.filter (\p -> not <| Dict.member p g.table) (grid (columns g))


match : Game -> List Pos -> Bool
match g ps =
    if List.length ps /= gameMatchSize g then
        False
    else
        let
            cards =
                List.filterMap (flip Dict.get g.table) ps
        in
        case g.type_ of
            Triples ->
                Card.triple cards

            Quadruples ->
                Card.quadruple cards


take : Game -> List Pos -> ( Bool, Game )
take g ps =
    if match g ps then
        ( True, { g | table = List.foldr (<|) g.table (List.map Dict.remove ps) } )
    else
        ( False, g )


count : Game -> Int
count g =
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

        quads xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    (List.map ((::) y) <| triples ys) ++ quads ys
    in
    List.length <|
        case g.type_ of
            Triples ->
                List.filter Card.triple <| triples cards

            Quadruples ->
                List.filter Card.quadruple <| quads cards
