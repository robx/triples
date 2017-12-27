module SvgSet exposing (..)

import Card exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)


color : Color -> String
color c =
    case c of
        Red ->
            "red"

        Green ->
            "green"

        Purple ->
            "purple"


draw : Card -> Svg msg
draw c =
    let
        col =
            color c.color
    in
    g
        []
        [ card
        , g [ transform "translate(30,30)", stroke col, fill "none" ] [ diamond ]
        , g [ transform "translate(30,60)", stroke col, fill col ] [ diamond ]
        ]


diamond =
    polygon [ points "-20,0 0,8 20,0 0,-8" ] []


card =
    rect
        [ x "0"
        , y "0"
        , width "50"
        , height "80"
        , rx "10"
        , ry "10"
        , stroke "black"
        , fill "white"
        ]
        []
