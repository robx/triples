module Game exposing (..)

import Card exposing (..)
import Random exposing (Generator)
import Random.List exposing (shuffle)


deck : List Card
deck =
    List.map fromInt (List.range 0 80)


shuffled : Generator (List Card)
shuffled =
    shuffle deck


deal : List Card -> ( List Card, List Card )
deal cs =
    ( List.take 12 cs, List.drop 12 cs )
