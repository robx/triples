module Encode
    exposing
        ( Element
        , Keyword
        , Symbol
        , Tag
        , bool
        , encode
        , float
        , int
        , keyword
        , list
        , mustKeyword
        , mustObject
        , mustSymbol
        , mustTag
        , mustTagged
        , object
        , string
        , symbol
        , tagged
        , toKeyword
        , toSymbol
        , toTag
        )

{-| Encode Elm values to EDN elements.

You can mostly just use this the way you use
<a href="http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode">Json.Encode</a>
to encode JSON values.


# Basics

@docs Element, encode


# Encoding basic types

@docs string, int, bool, float, symbol, keyword


# Containers

@docs list


# Objects and tags

@docs object, mustObject, tagged, mustTagged


# Symbols

@docs Symbol, toSymbol, mustSymbol
@docs Keyword, toKeyword, mustKeyword
@docs Tag, toTag, mustTag

-}

import Dict
import Json.Encode as Json
import Parse
import Parser exposing ((|.))
import Types


{-| An EDN element.

This can be encoded to a `String` via [`encode`](encode).

-}
type alias Element =
    Types.Element


{-| An EDN symbol. You can make these from strings using
`toSymbol` or `mustSymbol`.
-}
type Symbol
    = Symbol String


fromSymbol : Symbol -> String
fromSymbol (Symbol s) =
    s


{-| An EDN keyword. You can make these from strings using
`toKeyword` or `mustKeyword`.
-}
type Keyword
    = Keyword String


fromKeyword : Keyword -> String
fromKeyword (Keyword s) =
    s


{-| An EDN tag. You can make these from strings using
`toTag` or `mustTag`.
-}
type Tag
    = Tag String


fromTag : Tag -> String
fromTag (Tag s) =
    s


plainSymbol : String -> Bool
plainSymbol s =
    case Parser.run (Parse.plainSymbol |. Parser.end) s of
        Ok _ ->
            True

        Err _ ->
            False


plainTag : String -> Bool
plainTag s =
    case Parser.run (Parse.plainTag |. Parser.end) s of
        Ok _ ->
            True

        Err _ ->
            False


{-| Create a `Symbol` from an Elm `String`. This will fail
and return `Nothing` if the string is not a valid symbol, compare
the [EDN spec](https://github.com/edn-format/edn#symbols).

`nil`, `true` and `false` are not valid symbols.

-}
toSymbol : String -> Maybe Symbol
toSymbol s =
    if plainSymbol s && Parse.isSymbol s then
        Just (Symbol s)

    else
        Nothing


{-| Create a `Symbol` from an Elm `String`, crashing if
it is not a valid symbol.
-}
mustSymbol : String -> Symbol
mustSymbol s =
    case toSymbol s of
        Just ss ->
            ss

        Nothing ->
            Debug.crash <| "not a valid EDN symbol: " ++ s


{-| Create a `Keyword` from an Elm `String`. This allows the
same strings as `toSymbol`, in addition to `"nil"`, `"true"`
and `"false"`.
-}
toKeyword : String -> Maybe Keyword
toKeyword s =
    if plainSymbol s then
        Just (Keyword s)

    else
        Nothing


{-| Create a `Keyword` from an Elm `String`, crashing if
it is not a valid symbol.
-}
mustKeyword : String -> Keyword
mustKeyword s =
    case toKeyword s of
        Just ss ->
            ss

        Nothing ->
            Debug.crash <| "not a valid EDN symbol: " ++ s


{-| Create a `Tag` from an Elm `String`. This allows the
same strings as `toSymbol`, in addition to `"nil"`, `"true"`
and `"false"`.
-}
toTag : String -> Maybe Tag
toTag s =
    if plainTag s then
        Just (Tag s)

    else
        Nothing


{-| Create a `Tag` from an Elm `String`, crashing if
it is not a valid symbol.
-}
mustTag : String -> Tag
mustTag s =
    case toTag s of
        Just ss ->
            ss

        Nothing ->
            Debug.crash <| "not a valid EDN symbol: " ++ s


{-| Make an EDN string from an Elm `String`.
-}
string : String -> Element
string =
    Types.String


{-| Make an EDN symbol from a `Symbol`.
-}
symbol : Symbol -> Element
symbol =
    Types.Symbol << fromSymbol


{-| Make an EDN keyword from a `Keyword`.

    encode <| keyword  <| mustKeyword "nil"
    --> ":nil"

-}
keyword : Keyword -> Element
keyword =
    Types.Keyword << fromKeyword


{-| Make an EDN integer from an Elm `Int`.
-}
int : Int -> Element
int =
    Types.Int


{-| Make an EDN bool from an Elm `Bool`.
-}
bool : Bool -> Element
bool =
    Types.Bool


{-| Make an EDN floating point number from an Elm `Float`.
-}
float : Float -> Element
float =
    Types.Float


{-| Make an EDN list from an Elm `List` of EDN elements.
-}
list : List Element -> Element
list =
    Types.List


{-| Make an EDN object (map from keyword to element) from a list of
pairs of field name symbol and field value.

    case (toKeyword "x", toKeyword "y") of
        (Just x, Just y)
            -> encode (object [ (x, float 1.5), (y, float 0.0) ])
        _   -> ""
    --> "{:x 1.5, :y 0.0}"

-}
object : List ( Keyword, Element ) -> Element
object o =
    Types.Map
        (Dict.fromList <| List.map (\( s, e ) -> ( fromKeyword s, e )) <| o)
        []


{-| Make an EDN object (map from keyword to element) from a list of
pairs of field name string and field value. This will crash if any of
the field names are not valid EDN symbols.

    encode (mustObject [ ("x", float 1.5), ("y", float 0.0) ])
    --> "{:x 1.5, :y 0.0}"

-}
mustObject : List ( String, Element ) -> Element
mustObject =
    object << List.map (\( s, e ) -> ( mustKeyword s, e ))


{-| Encode an EDN element to EDN.

    encode (list [int 5, string "hello"])
    --> """(5 "hello")"""

-}
encode : Element -> String
encode e =
    case e of
        Types.Bool t ->
            if t then
                "true"

            else
                "false"

        Types.Int x ->
            toString x

        Types.Float x ->
            let
                f =
                    toString x
            in
            if String.any (\c -> c == 'e' || c == 'E' || c == '.') f then
                f

            else
                f ++ ".0"

        Types.String s ->
            Json.encode 0 (Json.string s)

        Types.Symbol s ->
            s

        Types.Keyword k ->
            ":" ++ k

        Types.List es ->
            "(" ++ String.join " " (List.map encode es) ++ ")"

        Types.Map keyed unkeyed ->
            let
                merged =
                    (Dict.toList keyed |> List.map (\( k, v ) -> ( Types.Keyword k, v ))) ++ unkeyed

                pair ( k, v ) =
                    encode k ++ " " ++ encode v
            in
            "{" ++ String.join ", " (List.map pair merged) ++ "}"

        Types.Tagged tag element ->
            "#" ++ tag ++ " " ++ encode element

        _ ->
            Debug.crash <| "unsupported element: " ++ toString e


{-| Make a tagged EDN element from a tag and an element.
-}
tagged : Tag -> Element -> Element
tagged t =
    Types.Tagged (fromTag t)


{-| Make a tagged EDN element from a `String` and an element.
This will crash if the tag is not a valid EDN symbol.

    type ID
        = UserID Int
        | Email String

    encode <| list <|
        List.map (\id -> case id of
            UserID i -> mustTagged "my/uid" (int i)
            Email e  -> mustTagged "my/email" (string e)
        ) [ UserID 5, Email "alice@example.com" ]
    --> """(#my/uid 5 #my/email "alice@example.com")"""

-}
mustTagged : String -> Element -> Element
mustTagged s =
    tagged (mustTag s)
