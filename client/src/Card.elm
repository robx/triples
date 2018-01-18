module Card exposing (..)

import List.Extra


type alias Card =
    { color : Int
    , count : Int
    , shape : Int
    , fill : Int
    }


fromInt : Int -> Card
fromInt x =
    let
        a1 =
            x % 3

        b1 =
            x // 3

        a2 =
            b1 % 3

        b2 =
            b1 // 3

        a3 =
            b2 % 3

        b3 =
            b2 // 3

        a4 =
            b3 % 3
    in
    { color = a1
    , count = a2
    , shape = a3
    , fill = a4
    }


toInt : Card -> Int
toInt c =
    c.color + 3 * c.count + 9 * c.shape + 27 * c.fill


triple : List Card -> Bool
triple cards =
    let
        test prop =
            cards
                |> List.map prop
                |> List.Extra.unique
                |> List.length
                |> flip List.member [ 1, 3 ]
    in
    (List.length cards == 3)
        && test .color
        && test .count
        && test .shape
        && test .fill


quadruple : List Card -> Bool
quadruple cards =
    case cards of
        [ c1, c2, c3, c4 ] ->
            let
                miss a b =
                    (-a - b) % 3

                missingCard a b =
                    { color = miss a.color b.color
                    , shape = miss a.shape b.shape
                    , count = miss a.count b.count
                    , fill = miss a.fill b.fill
                    }
            in
            (missingCard c1 c2 == missingCard c3 c4)
                || (missingCard c1 c3 == missingCard c2 c4)
                || (missingCard c1 c4 == missingCard c2 c3)

        _ ->
            False
