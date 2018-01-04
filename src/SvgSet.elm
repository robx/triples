module SvgSet exposing (..)

import Card exposing (..)
import D
import Svg exposing (..)
import Svg.Attributes exposing (..)


color : Int -> String
color c =
    case c of
        0 ->
            "rgb(229,46,37)"

        1 ->
            "rgb(72,128,52)"

        _ ->
            "rgb(116,44,177)"


draw : Bool -> Card -> Svg msg
draw selected c =
    let
        col =
            color c.color

        f =
            case c.fill of
                0 ->
                    col

                1 ->
                    "none"

                _ ->
                    "none"

        shade =
            case c.fill of
                1 ->
                    [ g
                        [ Svg.Attributes.clipPath <|
                            case c.shape of
                                0 ->
                                    "url(#clip0)"

                                1 ->
                                    "url(#clip1)"

                                _ ->
                                    "url(#clip2)"
                        , stroke col
                        , strokeWidth "0.3"
                        ]
                        [ shades ]
                    ]

                _ ->
                    []

        elt =
            case c.shape of
                0 ->
                    diamond

                1 ->
                    oval

                _ ->
                    squiggle

        locs =
            case c.count of
                0 ->
                    [ 0 ]

                1 ->
                    [ 10, -10 ]

                _ ->
                    [ 20, 0, -20 ]

        trans y =
            "translate(0," ++ toString y ++ ")"

        sym y =
            g [ transform (trans y) ]
                (shade ++ [ g [ stroke col, fill f ] [ elt ] ])
    in
    g
        []
        ([ card selected ] ++ List.map sym locs)


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


squiggle =
    Svg.path [ d squiggled ] []


shades =
    let
        s =
            String.join " " <|
                D.dM ( -17, 8 )
                    :: D.dl ( 0, -16 )
                    :: List.concat (List.repeat 34 [ D.dm ( 1, 16 ), D.dl ( 0, -16 ) ])
    in
    Svg.path [ d s ] []


clipPaths =
    [ Svg.clipPath [ id "clip0" ] [ diamond ]
    , Svg.clipPath [ id "clip1" ] [ oval ]
    , Svg.clipPath [ id "clip2" ] [ squiggle ]
    ]


dropShadow =
    Svg.filter [ id "dropshadow" ]
        [ Svg.feGaussianBlur [ in_ "SourceAlpha", stdDeviation "1" ] []
        , Svg.feOffset [ dx "2", dy "2", result "offsetblur" ] []
        , Svg.feComponentTransfer []
            [ Svg.feFuncA [ type_ "linear", slope "0.5" ] [] ]
        , Svg.feMerge []
            [ Svg.feMergeNode [] []
            , Svg.feMergeNode [ in_ "SourceGraphic" ] []
            ]
        ]


svgDefs =
    defs [] <| dropShadow :: clipPaths


card selected =
    rect
        [ x "-25"
        , y "-40"
        , width "50"
        , height "80"
        , rx "6"
        , ry "6"
        , stroke
            (if selected then
                "orange"
             else
                "slategray"
            )
        , strokeWidth
            (if selected then
                "1"
             else
                "0.2"
            )
        , fill "white"
        , Svg.Attributes.style "filter:url(#dropshadow)"
        ]
        []


letterCard : String -> Svg msg
letterCard c =
    g []
        [ rect
            [ x "-25"
            , y "-40"
            , width "50"
            , height "80"
            , rx "6"
            , ry "6"
            , stroke "none"
            , fill "lightgray"
            ]
            []
        , text_
            [ stroke "gray"
            , fill "gray"
            , textAnchor "middle"
            , fontSize "50"
            , Svg.Attributes.style "dominant-baseline: central;"
            ]
            [ text c ]
        ]


more : Svg msg
more =
    letterCard "+"
