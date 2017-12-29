module D
    exposing
        ( dC
        , dM
        , dS
        , dZ
        , dl
        , dm
        )


dd : String -> List ( Float, Float ) -> String
dd c ps =
    let
        ptos ( x, y ) =
            toString x ++ "," ++ toString y
    in
    String.join " " (c :: List.map ptos ps)


dl p =
    dd "l" [ p ]


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
