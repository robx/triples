module Graphics.SvgPath
    exposing
        ( dA
        , dC
        , dL
        , dM
        , dS
        , dZ
        , dl
        , dm
        )


ptos ( x, y ) =
    toString x ++ "," ++ toString y


dd : String -> List ( Float, Float ) -> String
dd c ps =
    String.join " " (c :: List.map ptos ps)


dl p =
    dd "l" [ p ]


dL p =
    dd "L" [ p ]


dm p =
    dd "m" [ p ]


dM p =
    dd "M" [ p ]


dC p q r =
    dd "C" [ p, q, r ]


dS p q =
    dd "S" [ p, q ]


dZ : String
dZ =
    dd "Z" []


dA : ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float ) -> String
dA p x q r =
    String.join " " [ "A", ptos p, toString x, ptos q, ptos r ]
