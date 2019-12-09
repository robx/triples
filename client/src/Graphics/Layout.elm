module Graphics.Layout exposing
    ( Layout
    , card
    , square
    )

import Graphics.Lib as Lib


type alias Layout =
    { locations : Int -> List String
    , w : Float
    , h : Float
    }


card : Layout
card =
    { locations = rectLocations
    , w = 50
    , h = 80
    }


square : Layout
square =
    { locations = squareLocations
    , w = 50
    , h = 50
    }


trans ( x, y ) =
    "translate(" ++ toString x ++ "," ++ toString y ++ ")"


squareLocations : Int -> List String
squareLocations count =
    List.map trans <|
        case count of
            0 ->
                [ ( 0, 0 ) ]

            1 ->
                Lib.rotate -45 <| Lib.scale 12 <| Lib.ngon 2

            _ ->
                Lib.rotate 15 <| Lib.scale 12 <| Lib.ngon 3


rectLocations : Int -> List String
rectLocations count =
    List.map trans <|
        case count of
            0 ->
                [ ( 0, 0 ) ]

            1 ->
                [ ( 0, 10 ), ( 0, -10 ) ]

            _ ->
                [ ( 0, 20 ), ( 0, 0 ), ( 0, -20 ) ]
