module Types exposing (..)

{-| EDN Types

@docs Element

-}

import Dict exposing (Dict)


{-| An EDN element
-}
type Element
    = Nil
    | Bool Bool
    | String String
    | Char Char
    | Symbol String
    | Keyword String
    | Int Int
    | BigInt { sign : String, digits : String }
    | Float Float
    | BigFloat { sign : String, digits : String } String { sign : String, digits : String }
    | Set (List Element)
    | List (List Element)
    | Vector (List Element)
    | Map (Dict String Element) (List ( Element, Element ))
    | Tagged String Element
