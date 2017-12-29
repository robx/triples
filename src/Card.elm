module Card exposing (..)


type Color
    = Red
    | Green
    | Purple


type Count
    = One
    | Two
    | Three


type Shape
    = Diamond
    | Oval
    | Squiggle


type Fill
    = Full
    | Shaded
    | Empty


type alias Card =
    { color : Color
    , count : Count
    , shape : Shape
    , fill : Fill
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
    { color =
        case a1 of
            0 ->
                Red

            1 ->
                Green

            _ ->
                Purple
            
    , count =
        case a2 of
            0 ->
                One

            1 ->
                Two

            _ ->
                Three
    , shape =
        case a3 of
            0 ->
                Diamond

            1 ->
                Oval

            _ ->
                Squiggle
    , fill =
        case a4 of
            0 ->
                Full

            1 ->
                Shaded

            _ ->
                Empty
    }
