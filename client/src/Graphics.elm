module Graphics exposing
    ( button
    , draw
    , svgDefs
    )

import Card exposing (..)
import Graphics.Colors as Colors
import Graphics.Layout as Layout
import Graphics.Lib as Lib
import Graphics.Shapes as Shapes
import Graphics.Style as Style
import Svg exposing (..)
import Svg.Attributes exposing (..)


lookup : ( a, a, a ) -> Int -> a
lookup ( x, y, z ) p =
    case p of
        0 ->
            x

        1 ->
            y

        _ ->
            z


draw : Style.Style -> Bool -> Card -> Svg msg
draw st selected c =
    let
        col =
            lookup st.colors.symbols c.color

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
                        [ Svg.map never st.shapes.shade ]
                    ]

                _ ->
                    []

        elt =
            Svg.map never <| lookup st.shapes.shapes c.shape

        sym t =
            g [ transform t ]
                (shade ++ [ g [ stroke col, strokeWidth "1.5", fill f ] [ elt ] ])
    in
    g
        []
        ([ card st selected ] ++ List.map sym (st.layout.locations c.count))


clipPaths : Style.Style -> List (Svg msg)
clipPaths st =
    let
        cp d =
            Svg.clipPath [ id <| "clip" ++ toString d ]
                [ Svg.map never <| lookup st.shapes.shapes d ]
    in
    List.map cp [ 0, 1, 2 ]


dropShadow : Svg msg
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


svgDefs : Style.Style -> Svg msg
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


cornerRadius : Float
cornerRadius =
    6


card : Style.Style -> Bool -> Svg msg
card st selected =
    rect
        (rectAttrs ( st.layout.w, st.layout.h ) cornerRadius
            ++ [ stroke
                    (if selected then
                        st.colors.select

                     else
                        st.colors.foreground
                    )
               , strokeWidth
                    (if selected then
                        "2.5"

                     else
                        "0.2"
                    )
               , fill st.colors.background
               , Svg.Attributes.style "filter: url(#dropshadow);"
               ]
        )
        []


button : Style.Style -> String -> Svg msg
button st c =
    g
        []
        [ rect
            (rectAttrs ( st.layout.w, st.layout.h ) cornerRadius
                ++ [ stroke "none"
                   , fill "lightgray"
                   ]
            )
            []
        , text_
            [ stroke "gray"
            , fill "gray"
            ]
            [ text c ]
        ]
