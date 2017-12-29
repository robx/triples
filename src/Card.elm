module Card exposing (..)


type Color
    = Red
    | Green
    | Purple


type Count
    = One
    | Two
    | Three


type Shape
    = Diamond
    | Oval
    | Squiggle


type Fill
    = Full
    | Shaded
    | Empty


type alias Card =
    { color : Color
    , count : Count
    , shape : Shape
    , fill : Fill
    }
