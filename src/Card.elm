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


set : List Card -> Bool
set cards =
    let
        test prop =
            cards
                |> List.map prop
                |> List.Extra.unique
                |> List.length
                |> flip List.member [ 1, 3 ]
    in
    List.length cards
        == 3
        && test .color
        && test .count
        && test .shape
        && test .fill
