module Graphics.Shapes
    exposing
        ( Shapes
        , classic
        , squared
        , variant
        )

import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgPath as D


type alias Shapes =
    { shapes : ( Svg Never, Svg Never, Svg Never )
    , shade : Svg Never
    }


classic : Shapes
classic =
    { shapes = ( diamond, oval, squiggle )
    , shade = shades
    }


variant : Shapes
variant =
    { shapes = ( rectangle, ellipse, lens )
    , shade = shades
    }


squared : Shapes
squared =
    { shapes = ( square, triangle, circle )
    , shade = shades
    }


ngon : Int -> List ( Float, Float )
ngon n =
    let
        z =
            2 * pi / toFloat n
    in
    List.range 0 (n - 1) |> List.map (\k -> ( cos (toFloat k * z), sin (toFloat k * z) ))


scale : Float -> List ( Float, Float ) -> List ( Float, Float )
scale f =
    List.map <| \( x, y ) -> ( f * x, f * y )


rotate : Float -> List ( Float, Float ) -> List ( Float, Float )
rotate a =
    let
        r =
            2 * pi * a / 360
    in
    List.map <| \( x, y ) -> ( cos r * x + sin r * y, sin r * x - cos r * y )


toPoints : List ( Float, Float ) -> String
toPoints =
    String.join " " << List.map (\( x, y ) -> toString x ++ "," ++ toString y)


square : Svg msg
square =
    polygon [ points <| toPoints <| scale 8 <| rotate 45 <| ngon 4 ] []


circle : Svg msg
circle =
    Svg.ellipse
        [ cx "0"
        , cy "0"
        , rx "7"
        , ry "7"
        ]
        []


triangle : Svg msg
triangle =
    polygon [ points <| toPoints <| scale 8.5 <| ngon 3 ] []


rectangle : Svg msg
rectangle =
    rect
        [ x "-18"
        , y "-6"
        , width "36"
        , height "12"
        ]
        []


ellipse : Svg msg
ellipse =
    Svg.ellipse
        [ cx "0"
        , cy "0"
        , rx "18"
        , ry "6"
        ]
        []


lens : Svg msg
lens =
    Svg.path
        [ d <|
            String.join " " <|
                [ D.dM ( -18, 6 )
                , D.dA ( 40, 40 ) 0 ( 0, 1 ) ( 18, 6 )
                , D.dL ( 18, -6 )
                , D.dA ( 40, 40 ) 0 ( 0, 1 ) ( -18, -6 )
                , D.dZ
                ]
        ]
        []


diamond : Svg msg
diamond =
    polygon [ points "-18,0 0,6 18,0 0,-6" ] []


oval : Svg msg
oval =
    rect
        [ x "-18"
        , y "-6"
        , width "36"
        , height "12"
        , rx "6"
        , ry "6"
        ]
        []


squiggle : Svg msg
squiggle =
    let
        ds =
            [ D.dM ( -18, 0 )
            , D.dC ( -18, -4 ) ( -12, -6.5 ) ( -10, -6.5 )
            , D.dC ( -3, -6.5 ) ( 1, -2.5 ) ( 7, -2.5 )
            , D.dC ( 11, -2.5 ) ( 13, -6 ) ( 15, -6 )
            , D.dS ( 18, -4 ) ( 18, 0 )
            , D.dS ( 12, 6.5 ) ( 10, 6.5 )
            , D.dC ( 3, 6.5 ) ( -1, 2.5 ) ( -7, 2.5 )
            , D.dC ( -11, 2.5 ) ( -13, 6 ) ( -15, 6 )
            , D.dS ( -18, 4 ) ( -18, 0 )
            , D.dZ
            ]
    in
    Svg.path [ d <| String.join " " ds ] []


shades : Svg msg
shades =
    let
        s =
            String.join " " <|
                D.dM ( -17, 8 )
                    :: D.dl ( 0, -16 )
                    :: List.concat (List.repeat 34 [ D.dm ( 1, 16 ), D.dl ( 0, -16 ) ])
    in
    Svg.path [ d s ] []
