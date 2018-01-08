module SvgSet exposing (..)

import Card exposing (..)
import D
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Style msg =
    { colors : ( String, String, String )
    , foreground : String
    , background : String
    , table : String
    , select : String
    , shapes : ( Svg msg, Svg msg, Svg msg )
    }


lookup : ( a, a, a ) -> Int -> a
lookup ( x, y, z ) p =
    case p of
        0 ->
            x

        1 ->
            y

        _ ->
            z


standardSet : Style msg
standardSet =
    { colors = ( "rgb(229,46,37)", "rgb(72,128,52)", "rgb(116,44,177)" )
    , foreground = "black"
    , background = "white"
    , table = "white"
    , select = "orange"
    , shapes = ( diamond, oval, squiggle )
    }


mySet : Style msg
mySet =
    { colors = ( "rgb(5,135,137)", "rgb(213,75,26)", "rgb(233,167,67)" )
    , foreground = "rgb(80,61,46)"
    , background = "rgb(246,242,231)"
    , table = "rgb(240,236,201)"
    , select = "orange"
    , shapes = ( rectangle, ellipse, lens )
    }


squareSet : Style msg
squareSet =
    { colors = mySet.colors
    , foreground = "rgb(80,61,46)"
    , background = "rgb(246,242,231)"
    , table = "rgb(240,236,201)"
    , select = "orange"
    , shapes = ( square, triangle, circle )
    }


draw : Layout msg -> Style msg -> Bool -> Card -> Svg msg
draw layout st selected c =
    let
        col =
            lookup st.colors c.color

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
            lookup st.shapes c.shape

        sym t =
            g [ transform t ]
                (shade ++ [ g [ stroke col, fill f ] [ elt ] ])
    in
    g
        []
        ([ layout.card st selected ] ++ List.map sym (layout.locations c.count))


type alias Layout msg =
    { locations : Int -> List String
    , card : Style msg -> Bool -> Svg msg
    , button : String -> Svg msg
    , w : Float
    , h : Float
    }


cardLayout : Layout msg
cardLayout =
    { locations = rectLocations
    , card = card ( 50, 80 ) 6
    , button = letterCard ( 50, 80 ) 6
    , w = 50
    , h = 80
    }


squareLayout : Layout msg
squareLayout =
    { locations = squareLocations
    , card = card ( 50, 50 ) 6
    , button = letterCard ( 50, 50 ) 6
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
                rotate -45 <| scale 12 <| ngon 2

            _ ->
                rotate 15 <| scale 12 <| ngon 3


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


squiggle : Svg msg
squiggle =
    Svg.path [ d squiggled ] []


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


clipPaths : Style msg -> List (Svg msg)
clipPaths st =
    let
        cp d =
            Svg.clipPath [ id <| "clip" ++ toString d ]
                [ lookup st.shapes d ]
    in
    List.map cp [ 0, 1, 2 ]


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


svgDefs : Style msg -> Svg msg
svgDefs st =
    defs [] <| dropShadow :: clipPaths st


rectAttrs : ( Float, Float ) -> Float -> List (Svg.Attribute msg)
rectAttrs ( w, h ) r =
    [ x (toString <| -w / 2)
    , y (toString <| -h / 2)
    , width (toString w)
    , height (toString h)
    , rx (toString r)
    , ry (toString r)
    ]


card : ( Float, Float ) -> Float -> Style msg -> Bool -> Svg msg
card ( w, h ) r st selected =
    rect
        (rectAttrs ( w, h ) r
            ++ [ stroke
                    (if selected then
                        st.select
                     else
                        st.foreground
                    )
               , strokeWidth
                    (if selected then
                        "1"
                     else
                        "0.2"
                    )
               , fill st.background
               , Svg.Attributes.style "filter: url(#dropshadow);"
               ]
        )
        []


letterCard : ( Float, Float ) -> Float -> String -> Svg msg
letterCard ( w, h ) r c =
    g
        []
        [ rect
            (rectAttrs ( w, h ) r
                ++ [ stroke "none"
                   , fill "lightgray"
                   ]
            )
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
