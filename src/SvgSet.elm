module SvgSet exposing (..)

import Card exposing (..)
import D
import Svg exposing (..)
import Svg.Attributes exposing (..)


color : Color -> String
color c =
    case c of
        Red ->
            "rgb(229,46,37)"

        Green ->
            "rgb(72,128,52)"

        Purple ->
            "rgb(116,44,177)"


draw : Card -> Svg msg
draw c =
    let
        col =
            color c.color

        f =
            case c.fill of
                Full ->
                    col

                Shaded ->
                    "none"

                Empty ->
                    "none"

        shade =
            case c.fill of
                Shaded ->
                    [ g
                        [ Svg.Attributes.clipPath <|
                            case c.shape of
                                Diamond ->
                                    "url(#diamond)"

                                Oval ->
                                    "url(#oval)"

                                Squiggle ->
                                    "url(#squiggle)"
                        , stroke col
                        , strokeWidth "0.3"
                        ]
                        [ shades ]
                    ]

                _ ->
                    []

        elt =
            case c.shape of
                Diamond ->
                    diamond

                Oval ->
                    oval

                Squiggle ->
                    squiggle

        locs =
            case c.count of
                One ->
                    [ 0 ]

                Two ->
                    [ 10, -10 ]

                Three ->
                    [ 20, 0, -20 ]

        trans y =
            "translate(0," ++ toString y ++ ")"

        sym y =
            g [ transform (trans y) ]
                (shade ++ [ g [ stroke col, fill f ] [ elt ] ])
    in
    g
        []
        ([ card ] ++ List.map sym locs)


diamond =
    polygon [ points "-18,0 0,6 18,0 0,-6" ] []


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


squiggled =
    String.join " "
        [ D.dM ( -18, 0 )
        , D.dC ( -18, -4 ) ( -12, -6 ) ( -10, -6 )
        , D.dS ( 2, -2 ) ( 6, -2 )
        , D.dS ( 13, -6 ) ( 15, -6 )
        , D.dS ( 18, -4 ) ( 18, 0 )
        , D.dS ( 12, 6 ) ( 10, 6 )
        , D.dS ( -2, 2 ) ( -6, 2 )
        , D.dS ( -13, 6 ) ( -15, 6 )
        , D.dS ( -18, 4 ) ( -18, 0 )
        , D.dZ
        ]


squiggle =
    Svg.path [ d squiggled ] []


shades =
    let
        s =
            String.join " " <|
                D.dM ( -17, 6 )
                    :: D.dl ( 0, -12 )
                    :: List.concat (List.repeat 34 [ D.dm ( 1, 12 ), D.dl ( 0, -12 ) ])
    in
    Svg.path [ d s ] []


clipPaths =
    defs []
        [ Svg.clipPath [ id "diamond" ] [ diamond ]
        , Svg.clipPath [ id "oval" ] [ oval ]
        , Svg.clipPath [ id "squiggle" ] [ squiggle ]
        ]


card =
    rect
        [ x "-25"
        , y "-40"
        , width "50"
        , height "80"
        , rx "6"
        , ry "6"
        , stroke "black"
        , strokeWidth "0.8"
        , fill "white"
        ]
        []
