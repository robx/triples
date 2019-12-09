module Graphics.Lib exposing
    ( ngon
    , rotate
    , scale
    )


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
