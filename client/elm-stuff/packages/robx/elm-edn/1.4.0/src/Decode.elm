module Decode
    exposing
        ( Decoder
        , andThen
        , at
        , bool
        , char
        , decodeString
        , dict
        , fail
        , field
        , float
        , index
        , instant
        , int
        , keyValuePairs
        , keyword
        , lazy
        , list
        , map
        , map2
        , map3
        , map4
        , map5
        , map6
        , map7
        , map8
        , nil
        , oneOf
        , optional
        , optionalField
        , set
        , string
        , succeed
        , symbol
        , tag
        , tagged
        , uuid
        , vector
        )

{-| Build decoders for [EDN](https://github.com/edn-format/edn) elements.

You can mostly just use this the way you use
<a href="http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode">Json.Decode</a>
to decode JSON values.


# Primitives

@docs Decoder
@docs bool, int, float, string, char
@docs nil, symbol, keyword


# Data Structures

@docs optional, list, vector, set, keyValuePairs, dict
@docs index


# Object Primitives

@docs field, optionalField
@docs tagged
@docs at


# Inconsistent Structure

@docs oneOf


# Run Decoders

@docs decodeString


# Mapping

@docs map, map2, map3, map4, map5, map6, map7, map8


# Fancy Decoding

@docs lazy, succeed, fail, andThen


# Custom Types

@docs tag, instant, uuid

-}

import Date
import Dict
import Parse
import Parser
import Regex
import Set
import Types exposing (..)


{-| A value that knows how to decode EDN elements.
-}
type alias Decoder a =
    Element -> Result String a


{-| Parse the given string into a single EDN element and
and decode it to an Elm value using the given Decoder.
-}
decodeString : Decoder a -> String -> Result String a
decodeString d s =
    Parser.run Parse.onlyElement s
        |> Result.mapError toString
        |> Result.andThen d


{-| Transform a decoder.
-}
map : (a -> value) -> Decoder a -> Decoder value
map f d =
    Result.map f << d


{-| Try two decoders and then combine the result. We can use this to decode
objects with many fields:

    type alias Point =
        { x : Float, y : Float }

    point : Decoder Point
    point =
        map2 Point
            (field "x" float)
            (field "y" float)

    decodeString point """{:x 3.0, :y 4.0}"""
    --> Ok { x = 3, y = 4 }

It tries each individual decoder and puts the result together with the `Point`
constructor.

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 f d1 d2 e =
    Result.map2 f (d1 e) (d2 e)


{-| -}
map3 : (a -> b -> c -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder value
map3 f d1 d2 d3 e =
    Result.map3 f (d1 e) (d2 e) (d3 e)


{-| -}
map4 : (a -> b -> c -> d -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder value
map4 f d1 d2 d3 d4 e =
    Result.map4 f (d1 e) (d2 e) (d3 e) (d4 e)


{-| -}
map5 : (a -> b -> c -> d -> e -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder value
map5 f d1 d2 d3 d4 d5 =
    map4 f d1 d2 d3 d4 |> andThen (\g -> map g d5)


{-| -}
map6 : (a -> b -> c -> d -> e -> f -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder value
map6 f d1 d2 d3 d4 d5 d6 =
    map5 f d1 d2 d3 d4 d5
        |> andThen (\g -> map g d6)


{-| -}
map7 : (a -> b -> c -> d -> e -> f -> g -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder value
map7 f d1 d2 d3 d4 d5 d6 d7 =
    map6 f d1 d2 d3 d4 d5 d6
        |> andThen (\g -> map g d7)


{-| -}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> value) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e -> Decoder f -> Decoder g -> Decoder h -> Decoder value
map8 f d1 d2 d3 d4 d5 d6 d7 d8 =
    map7 f d1 d2 d3 d4 d5 d6 d7
        |> andThen (\g -> map g d8)


{-| -}
lazy : (() -> Decoder a) -> Decoder a
lazy f =
    succeed () |> andThen f


{-| Ignore the input and make the decoder succeed with the given value.
-}
succeed : a -> Decoder a
succeed x =
    always (Ok x)


{-| Ignore the input and make the decoder fail with the given message.
-}
fail : String -> Decoder a
fail err =
    always (Err err)


{-| Create a decoder that depends on the result of a previous decoder.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f p e =
    p e |> Result.andThen (\x -> f x e)


context : String -> Decoder a -> Decoder a
context ctx d =
    Result.mapError (\e -> "while decoding " ++ ctx ++ ", " ++ e) << d


wrongType : Element -> Element -> Result String a
wrongType want have =
    wrongTypeMany [ want ] have


wrongTypeMany : List Element -> Element -> Result String a
wrongTypeMany want have =
    let
        desc el =
            case el of
                String _ ->
                    "a string"

                Int _ ->
                    "an integer"

                Nil ->
                    "`nil`"

                Symbol _ ->
                    "a symbol"

                Keyword _ ->
                    "a keyword"

                Tagged _ _ ->
                    "a tagged element"

                List _ ->
                    "a list"

                Set _ ->
                    "a set"

                Map _ _ ->
                    "a map"

                Float _ ->
                    "a floating point number"

                BigInt _ ->
                    "a big integer"

                BigFloat _ _ _ ->
                    "a big floating point number"

                Bool _ ->
                    "a boolean"

                Char _ ->
                    "a character"

                Vector _ ->
                    "a vector"
    in
    Err <|
        "expected "
            ++ (String.join " or " <| List.map desc want)
            ++ " but found "
            ++ desc have


{-| Decode an EDN character into an Elm `Char`.

    decodeString char "\\a"       --> Ok 'a'
    decodeString char "\\newline" --> Ok '\n'
    decodeString char "\\\\"      --> Ok '\\'
    decodeString char "\\u0026"   --> Ok '&'

-}
char : Decoder Char
char e =
    case e of
        Char c ->
            Ok c

        _ ->
            wrongType (Char ' ') e


{-| Decode an EDN string into an Elm `String`.

    decodeString string "\"12345\"" --> Ok "12345"
    decodeString string "\"😞\""    --> Ok "😞"
    decodeString string "12345"     --> Err "expected a string but found an integer"

-}
string : Decoder String
string e =
    case e of
        String s ->
            Ok s

        _ ->
            wrongType (String "") e


{-| Decode an EDN integer into an Elm `Int`.

    decodeString int "12345" --> Ok 12345
    decodeString int "1.0"   --> Err "expected an integer but found a floating point number"

-}
int : Decoder Int
int e =
    case e of
        Int x ->
            Ok x

        _ ->
            wrongType (Int 0) e


{-| Decode an EDN floating point number into an Elm `Float`

     decodeString float "1.1" --> Ok 1.1
     decodeString float "1E1" --> Ok 10.0
     decodeString float "2"
     --> Err "expected a floating point number but found an integer"

-}
float : Decoder Float
float e =
    case e of
        Float x ->
            Ok x

        _ ->
            wrongType (Float 0) e


{-| Decode an EDN nil value.

     decodeString keyword ":nil" --> Ok "nil"
     decodeString keyword "yo"   --> Err "expected a keyword but found a symbol"
     decodeString keyword "nil"  --> Err "expected a keyword but found `nil`"

-}
nil : Decoder ()
nil e =
    case e of
        Nil ->
            Ok ()

        _ ->
            wrongType Nil e


{-| Decode an EDN symbol into an Elm `String`.

     decodeString symbol "yo"     --> Ok "yo"
     decodeString symbol ":black" --> Err "expected a symbol but found a keyword"

-}
symbol : Decoder String
symbol e =
    case e of
        Symbol s ->
            Ok s

        _ ->
            wrongType (Symbol "") e


{-| Decode an EDN keyword into an Elm `String`.

     decodeString keyword ":black" --> Ok "black"
     decodeString keyword "yo"     --> Err "expected a keyword but found a symbol"
     decodeString keyword ":true"  --> Ok "true"

-}
keyword : Decoder String
keyword e =
    case e of
        Keyword s ->
            Ok s

        _ ->
            wrongType (Keyword "") e


{-| Decode an EDN boolean into an Elm `Bool`.

     decodeString bool "true"   --> Ok True
     decodeString bool "false"  --> Ok False
     decodeString bool "nil"    --> Err "expected a boolean but found `nil`"
     decodeString bool "(true)" --> Err "expected a boolean but found a list"

-}
bool : Decoder Bool
bool e =
    case e of
        Bool b ->
            Ok b

        _ ->
            wrongType (Bool False) e


seq : Decoder a -> List Element -> Result String (List a)
seq d l =
    case l of
        f :: fs ->
            Result.map2 (::) (d f) (seq d fs)

        [] ->
            Ok []


{-| Decode an EDN nil value into `Nothing`, or any other value into an Elm value.

     decodeString (optional bool) "true" --> Ok (Just True)
     decodeString (optional bool) "nil"  --> Ok Nothing

-}
optional : Decoder a -> Decoder (Maybe a)
optional d =
    oneOf
        [ map (always Nothing) nil
        , map Just d
        ]


{-| Decode an EDN list into an Elm `List`.

    decodeString
        (list int)
        "(1 2 3 4 5)"
    --> Ok [1, 2, 3, 4, 5]

    decodeString
        (list (list string))
        """(() ("hello" "world") ())"""
    --> Ok [[], ["hello", "world"], []]

-}
list : Decoder a -> Decoder (List a)
list d e =
    case e of
        List l ->
            seq d l

        _ ->
            wrongType (List []) e


{-| Decode an EDN vector into an Elm `List`.

    decodeString (vector int) "[2 2]"
    --> Ok [2, 2]

-}
vector : Decoder a -> Decoder (List a)
vector d e =
    case e of
        Vector v ->
            seq d v

        _ ->
            wrongType (Vector []) e


{-| Decode an EDN set into an Elm `Set`.

    import Set

    decodeString (set int) "#{1, 2}"
    --> Ok (Set.fromList [1, 2])

-}
set : Decoder comparable -> Decoder (Set.Set comparable)
set d e =
    case e of
        Set s ->
            seq d s |> Result.map Set.fromList

        _ ->
            wrongType (Set []) e


{-| Decode an EDN map into an Elm `List` of pairs. If the keys are
simple (strings, integers, keywords, ...), consider using
[`dict`](#dict) or [`field`](#field) instead.

    decodeString
        (keyValuePairs (list int) string)
        """{(1 2) "onetwo"
            (2 1) "twoone"
            ()    "three"}"""
    --> Ok [ ([1, 2], "onetwo")
    -->    , ([2, 1], "twoone")
    -->    , ([], "three")
    -->    ]

-}
keyValuePairs : Decoder key -> Decoder value -> Decoder (List ( key, value ))
keyValuePairs key value e =
    let
        merge keyed unkeyed =
            (Dict.toList keyed |> List.map (\( k, v ) -> ( Keyword k, v ))) ++ unkeyed
    in
    case e of
        Map keyed unkeyed ->
            let
                rec xs =
                    case xs of
                        ( k, v ) :: ys ->
                            Result.map2 (,) (key k) (value v)
                                |> Result.andThen (\x -> Result.map ((::) x) (rec ys))

                        [] ->
                            Ok []
            in
            rec (merge keyed unkeyed)

        _ ->
            wrongType (Map Dict.empty []) e


{-| Decode an EDN map into an Elm `Dict`.

    import Dict

    decodeString
        (dict int string)
        """{12 "onetwo", 21 "twoone", 3 "three"}"""
    --> Ok <| Dict.fromList
    -->     [ (12, "onetwo")
    -->     , (21, "twoone")
    -->     , (3,  "three")
    -->     ]

-}
dict : Decoder comparable -> Decoder value -> Decoder (Dict.Dict comparable value)
dict key value =
    map Dict.fromList (keyValuePairs key value)


{-| Decode an object encoded as a map from keywords to element
-}
object : Decoder (Dict.Dict String Element)
object e =
    case e of
        Map keyed [] ->
            Ok keyed

        Map _ unkeyed ->
            Err <| "non-keyword keys in map: " ++ toString unkeyed

        _ ->
            wrongType (Map Dict.empty []) e


{-| Decode an optional EDN map field.

    decodeString (optionalField "twitter" string)
        """{:name "Alice", :twitter "@alice"}"""
    --> Ok (Just "@alice")

    decodeString (optionalField "twitter" string)
        """{:name "Bob", :icq 12345678}"""
    --> Ok Nothing

    decodeString (optionalField "twitter" string)
        """{:name "Eve", :twitter nil}"""
    --> Err "expected a string but found `nil`"

-}
optionalField : String -> Decoder a -> Decoder (Maybe a)
optionalField f d =
    map (Dict.get f) object
        |> andThen
            (\maybeElement _ ->
                case maybeElement of
                    Just e ->
                        Result.map Just (d e)

                    Nothing ->
                        Ok Nothing
            )


{-| Decode an EDN map field.

    decodeString (field "twitter" string)
        """{:name "Alice", :twitter "@alice"}"""
    --> Ok "@alice"

    decodeString (field "twitter" string)
        """{:name "Bob", :icq 12345678}"""
    --> Err "field not found: twitter"

    decodeString (field "twitter" (optional string))
        """{:name "Alice", :twitter "@alice"}"""
    --> Ok (Just "@alice")

    decodeString (field "twitter" (optional string))
        """{:name "Eve", :twitter nil}"""
    --> Ok Nothing

-}
field : String -> Decoder a -> Decoder a
field f d =
    optionalField f d
        |> andThen
            (\m ->
                case m of
                    Just x ->
                        succeed x

                    Nothing ->
                        fail <| "field not found: " ++ f
            )


{-| Decode a nested object, requiring certain fields.

    decodeString (at ["end", "x"] float)
        """{:start {:x 1.0, :y 1.5}
            :end   {:x 0.5, :y 0.334}}"""
    --> Ok 0.5

    decodeString (at ["start", "z"] float)
        """{:start {:x 1.0, :y 1.5}
            :end   {:x 0.5, :y 0.334}}"""
    --> Err "field not found: z"

-}
at : List String -> Decoder a -> Decoder a
at path d =
    case path of
        f :: fs ->
            field f (at fs d)

        [] ->
            d


{-| Decode an EDN vector, requiring a particular index.

    decodeString (index 1 int)
        """["one" 2 3.0 nil]"""
    --> Ok 2
    decodeString (index 1 int)
        "[1]"
    --> Err "index 1 not found in vector of length 1"

This will fail on EDN lists.

    decodeString (index 1 int)
        "(1 2 3)"
    --> Err "expected a vector but found a list"

-}
index : Int -> Decoder a -> Decoder a
index i d e =
    case e of
        Vector v ->
            case v |> List.drop i |> List.head of
                Just el ->
                    d el

                Nothing ->
                    Err <| "index " ++ toString i ++ " not found in vector of length " ++ toString (List.length v)

        _ ->
            wrongType (Vector []) e


{-| Decode a tagged element.

    decodeString (tag "my/tag" int) "#my/tag 55"
    --> Ok 55
    decodeString (tag "my/tag" int) "#your/tag 55"
    --> Err "expected tag `my/tag` but found `your/tag`"
    decodeString (tag "my/tag" int) "55"
    --> Err "expected a tagged element but found an integer"

-}
tag : String -> Decoder a -> Decoder a
tag t d e =
    case e of
        Tagged tt f ->
            if t == tt then
                d f

            else
                Err <| "expected tag `" ++ t ++ "` but found `" ++ tt ++ "`"

        _ ->
            wrongType (Tagged "" Nil) e


{-| Decode an element based on its tag.

    type ID
        = UserID Int
        | Email String

    decodeString
        (list <| tagged
            [ ( "my/uid",   map UserID int   )
            , ( "my/email", map Email string )
            ])
        """(#my/uid 1, #my/email "alice@example.com", #my/uid 334)"""
    --> Ok <|
    -->     [ UserID 1
    -->     , Email "alice@example.com"
    -->     , UserID 334
    -->     ]

-}
tagged : List ( String, Decoder a ) -> Decoder a
tagged decoders e =
    case e of
        Tagged t f ->
            case Dict.get t (Dict.fromList decoders) of
                Just d ->
                    context ("an element tagged #" ++ t) d <| f

                Nothing ->
                    Err <| "unexpected tag: " ++ t

        _ ->
            wrongType (Tagged "" Nil) e


{-| Try a bunch of different decoders, one after the other. The
result is the value of the first successful decoder.

    type ID
        = UserID Int
        | Email String

    decodeString
        (list <| oneOf
            [ map UserID int
            , map Email string
            ])
        """(1, "alice@example.com", 334)"""
    --> Ok
    -->     [ UserID 1
    -->     , Email "alice@example.com"
    -->     , UserID 334
    -->     ]

-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders e =
    case decoders of
        d :: ds ->
            case d e of
                Ok v ->
                    Ok v

                Err _ ->
                    oneOf ds e

        [] ->
            Err "oneOf: all decoders failed"


{-| Decode an EDN instant to an Elm `Date`. An EDN instant
is a tagged string of an ISO 8601 date.

    import Date

    decodeString instant
        "#inst \"1985-04-12T23:20:50.52Z\""
    --> Date.fromString "1985-04-12T23:20:50.52Z"
    decodeString instant
        "#inst \"1985-04-12T23:20:50.52+04:00\""
    --> Date.fromString "1985-04-12T23:20:50.52+04:00"
    decodeString instant
        "\"1985-04-12T23:20:50.52Z\""
    --> Err "expected a tagged element but found a string"

-}
instant : Decoder Date.Date
instant =
    tag "inst" string
        |> andThen
            (\s ->
                case Date.fromString s of
                    Ok d ->
                        succeed d

                    Err e ->
                        fail e
            )


{-| Decode an EDN UUID to an Elm `String`.

    decodeString uuid
        "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
    --> Ok "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"
    decodeString uuid
        "#uuid \"F81D4FAE-7DEC-11D0-A765-00A0C91E6BF6\""
    --> Err "invalid UUID"
    decodeString uuid
        "\"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
    --> Err "expected a tagged element but found a string"

-}
uuid : Decoder String
uuid =
    let
        r =
            Regex.regex "^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"
    in
    tag "uuid" string
        |> andThen
            (\s ->
                if Regex.contains r s then
                    succeed s

                else
                    fail "invalid UUID"
            )
