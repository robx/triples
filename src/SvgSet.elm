module SvgSet exposing (..)

import Card exposing (..)
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
                        , strokeWidth "0.5"
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
              (shade ++ [g [ stroke col, fill f ] [ elt ]])
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


squiggle =
    Svg.path [ d "M-18,0 C-18,-4 -12,-6 -10,-6 S-2,-4 0,-3 S 4,-2 6,-2 S13,-6 15,-6 S18,-4 18,0 S12,6 10,6 S2,4 0,3 S-4,2 -6,2 S-13,6 -15,6 S-18,4 -18,0 Z" ] []


shades =
    Svg.path [ d "M-17,6 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12 m2,12 l0,-12" ] []


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
