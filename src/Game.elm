module Game exposing (..)

import Card exposing (..)


deck : List Card
deck =
    List.map fromInt (List.range 0 80)
