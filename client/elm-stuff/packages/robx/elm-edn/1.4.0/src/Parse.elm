module Parse exposing (isSymbol, onlyElement, onlyElements, plainSymbol, plainTag)

{-| Parsing EDN


# Basic parsers

@docs onlyElement, onlyElements, plainSymbol

-}

import Char
import Dict
import List
import Parser as P exposing ((|.), (|=), Parser)
import String
import Types exposing (..)


seq : String -> String -> Parser (List Element)
seq open close =
    P.succeed identity
        |. P.symbol open
        |. junk
        |= P.lazy (\_ -> elements)
        |. P.symbol close


discard : Parser ()
discard =
    P.inContext "discard" <|
        P.symbol "#_"
            |. P.lazy (\_ -> junk)
            |. P.lazy (\_ -> element)


comment : Parser ()
comment =
    P.symbol ";"
        |. P.ignore P.zeroOrMore (\c -> c /= '\n')


{-| Parse any number of EDN elements.
-}
elements : Parser (List Element)
elements =
    P.repeat P.zeroOrMore (P.lazy (\_ -> element))


{-| Parse a single EDN element.
-}
onlyElement : Parser Element
onlyElement =
    P.succeed identity
        |. junk
        |= element
        |. P.end


{-| Parse any number of EDN elements.
-}
onlyElements : Parser (List Element)
onlyElements =
    P.succeed identity
        |. junk
        |= elements
        |. P.end


element : Parser Element
element =
    P.oneOf
        [ P.succeed identity
            |. P.lazy (\_ -> discard)
            |= P.lazy (\_ -> element)
        , P.succeed identity
            |. comment
            |= P.lazy (\_ -> element)
        , P.lazy (\_ -> list)
        , P.lazy (\_ -> vector)
        , P.lazy (\_ -> map)
        , P.lazy (\_ -> set)
        , P.lazy (\_ -> tagged) -- commits on '#', so must be after discard and set
        , number |. sep
        , string
        , symbols |. sep
        , ednKeyword |. sep
        , char |. sep
        ]
        |. P.lazy (\_ -> junk)


sep : Parser ()
sep =
    P.oneOf
        [ spaceSep
        , P.end
        , P.lookAhead <|
            P.oneOf
                [ P.symbol "("
                , P.symbol "["
                , P.symbol "{"
                , P.symbol ")"
                , P.symbol "]"
                , P.symbol "}"
                , P.symbol "\""
                , P.symbol ";"
                ]
        ]


isSpace : Char -> Bool
isSpace c =
    String.contains (String.fromChar c) ", \t\n\x0D"


space : Parser ()
space =
    P.ignore P.zeroOrMore isSpace


spaceSep : Parser ()
spaceSep =
    P.ignore P.oneOrMore isSpace


junk : Parser ()
junk =
    P.succeed ()
        |. P.repeat P.zeroOrMore
            (P.oneOf
                [ P.lazy (\_ -> discard)
                , comment
                , spaceSep
                ]
            )


unicodeEscape : Parser Char
unicodeEscape =
    let
        onehex c =
            case Char.toLower c of
                '0' ->
                    0

                '1' ->
                    1

                '2' ->
                    2

                '3' ->
                    3

                '4' ->
                    4

                '5' ->
                    5

                '6' ->
                    6

                '7' ->
                    7

                '8' ->
                    8

                '9' ->
                    9

                'a' ->
                    10

                'b' ->
                    11

                'c' ->
                    12

                'd' ->
                    13

                'e' ->
                    14

                'f' ->
                    15

                _ ->
                    0

        hexrev s =
            case String.uncons s of
                Just ( c, rest ) ->
                    onehex c + 16 * hexrev rest

                Nothing ->
                    0
    in
    P.succeed (Char.fromCode << hexrev << String.reverse)
        |. P.symbol "u"
        |= P.keep (P.Exactly 4) Char.isHexDigit


{-| Parses an EDN string
-}
string : Parser Element
string =
    let
        esc c =
            case c of
                "t" ->
                    "\t"

                "n" ->
                    "\n"

                "r" ->
                    "\x0D"

                _ ->
                    c

        part =
            P.oneOf
                [ P.keep P.oneOrMore (\c -> not (String.contains (String.fromChar c) "\\\""))
                , P.succeed identity
                    |. P.symbol "\\"
                    |= P.oneOf
                        [ P.succeed String.fromChar
                            |= unicodeEscape
                        , P.succeed esc
                            |= P.keep (P.Exactly 1) (always True)
                        ]
                ]
    in
    P.succeed (String << String.concat)
        |. P.symbol "\""
        |= P.repeat P.zeroOrMore part
        |. P.symbol "\""


char : Parser Element
char =
    let
        stringToChar s =
            case String.uncons s of
                Just ( c, "" ) ->
                    c

                _ ->
                    Debug.crash "bad single-char string"
    in
    P.succeed Char
        |. P.symbol "\\"
        |= P.oneOf
            [ P.succeed '\n' |. P.keyword "newline"
            , P.succeed '\x0D' |. P.keyword "return"
            , P.succeed ' ' |. P.keyword "space"
            , P.succeed '\t' |. P.keyword "tab"
            , unicodeEscape
            , P.succeed stringToChar
                |= P.keep (P.Exactly 1) (always True)
            ]


list : Parser Element
list =
    P.inContext "list" <|
        P.succeed List
            |= (P.lazy <| \_ -> seq "(" ")")


vector : Parser Element
vector =
    P.inContext "vector" <|
        P.succeed Vector
            |= (P.lazy <| \_ -> seq "[" "]")


map : Parser Element
map =
    let
        build keyed unkeyed elements =
            case elements of
                key :: value :: rest ->
                    case key of
                        Keyword keyword ->
                            build (Dict.insert keyword value keyed) unkeyed rest

                        _ ->
                            build keyed (( key, value ) :: unkeyed) rest

                [] ->
                    P.succeed <| Map keyed (List.reverse unkeyed)

                _ ->
                    P.fail "uneven number of map elements"
    in
    P.inContext "map" <|
        ((P.lazy <| \_ -> seq "{" "}")
            |> P.andThen (build Dict.empty [])
        )


set : Parser Element
set =
    P.inContext "set" <|
        P.succeed Set
            |= (P.lazy <| \_ -> seq "#{" "}")


{-| plainSymbol parses the symbol-part of EDN symbols and keywords.
-}
plainSymbol : Parser String
plainSymbol =
    plainSymbolHelper True


{-| plainTag parses the symbol-part of EDN tags.
-}
plainTag : Parser String
plainTag =
    plainSymbolHelper False


plainSymbolHelper : Bool -> Parser String
plainSymbolHelper startUnderscore =
    let
        class s c =
            String.contains (String.fromChar c) s

        nosecondnum =
            "-+."

        notfirst =
            if startUnderscore then
                ":#"

            else
                "_:#"

        other =
            if startUnderscore then
                "_*!?$%&=<>"

            else
                "*!?$%&=<>"

        part =
            P.oneOf
                [ P.succeed (++)
                    |= P.keep
                        (P.Exactly 1)
                        (\c -> Char.isUpper c || Char.isLower c || class other c)
                    |= P.keep
                        P.zeroOrMore
                        (\c -> Char.isUpper c || Char.isLower c || Char.isDigit c || class (nosecondnum ++ notfirst ++ other) c)
                , P.succeed (++)
                    |= P.keep (P.Exactly 1) (class nosecondnum)
                    |= P.oneOf
                        [ P.succeed (++)
                            |= P.keep
                                (P.Exactly 1)
                                (\c -> Char.isUpper c || Char.isLower c || class (notfirst ++ other) c)
                            |= P.keep
                                P.zeroOrMore
                                (\c -> Char.isUpper c || Char.isLower c || Char.isDigit c || class (nosecondnum ++ notfirst ++ other) c)
                        , P.succeed ""
                        ]
                ]
    in
    P.oneOf
        [ P.succeed "/"
            |. P.symbol "/"
        , P.succeed (++)
            |= part
            |= P.oneOf
                [ P.succeed ((++) "/")
                    |. P.symbol "/"
                    |= part
                , P.succeed ""
                ]
        ]


fromSymbol : String -> Element
fromSymbol s =
    case s of
        "true" ->
            Bool True

        "false" ->
            Bool False

        "nil" ->
            Nil

        _ ->
            Symbol s


{-| Determines whether the argument is a real symbol,
as opposed to `nil`, `true`, `false`, given it is syntactically
a symbol
-}
isSymbol : String -> Bool
isSymbol s =
    case fromSymbol s of
        Symbol _ ->
            True

        _ ->
            False


{-| symbols parses EDN symbols and true/false/nil
-}
symbols : Parser Element
symbols =
    P.succeed fromSymbol
        |= plainSymbol


ednKeyword : Parser Element
ednKeyword =
    P.succeed Keyword
        |. P.symbol ":"
        |= plainSymbol


tagged : Parser Element
tagged =
    P.inContext "tagged" <|
        P.succeed Tagged
            |. P.symbol "#"
            |= plainTag
            |. sep
            |= P.lazy (\_ -> element)


type alias RawInteger =
    { sign : String
    , digits : String
    }


type alias RawNumber =
    { integer : RawInteger
    , frac : Maybe String
    , exp : Maybe RawInteger
    , big : Maybe String
    }


rawNumber : Parser RawNumber
rawNumber =
    let
        sign =
            P.oneOf
                [ P.keep (P.Exactly 1) (\c -> String.contains (String.fromChar c) "+-")
                , P.succeed "+"
                ]

        digits =
            P.oneOf
                [ P.keep (P.Exactly 1) (\c -> c == '0')
                , P.succeed (++)
                    |= P.keep (P.Exactly 1) (\c -> Char.isDigit c && c /= '0')
                    |= P.keep P.zeroOrMore (\c -> Char.isDigit c)
                ]

        integer =
            P.succeed RawInteger
                |= sign
                |= digits

        frac =
            P.succeed identity
                |. P.symbol "."
                |= P.keep P.zeroOrMore Char.isDigit

        exp =
            P.succeed RawInteger
                |. P.oneOf [ P.symbol "e", P.symbol "E" ]
                |= sign
                |= digits

        big =
            P.keep (P.Exactly 1) (\c -> c == 'M' || c == 'N')
    in
    P.succeed RawNumber
        |= integer
        |= P.oneOf [ P.map Just frac, P.succeed Nothing ]
        |= P.oneOf [ P.map Just exp, P.succeed Nothing ]
        |= P.oneOf [ P.map Just big, P.succeed Nothing ]


number : Parser Element
number =
    let
        rawToStr raw =
            raw.sign ++ raw.digits

        rawToInt raw =
            case String.toInt (rawToStr raw) of
                Ok v ->
                    v

                Err err ->
                    Debug.crash <| "failed to parse integer: " ++ rawToStr raw ++ ": " ++ err

        strToFloat s =
            case String.toFloat s of
                Ok v ->
                    v

                Err err ->
                    Debug.crash <| "failed to parse float: " ++ s ++ ": " ++ err

        f n =
            case ( n.frac, n.exp, n.big ) of
                ( Nothing, Nothing, Nothing ) ->
                    P.succeed <| Int <| rawToInt n.integer

                ( Nothing, Nothing, Just "N" ) ->
                    P.succeed <| BigInt n.integer

                ( _, _, Nothing ) ->
                    P.succeed <|
                        Float <|
                            strToFloat <|
                                String.concat
                                    [ n.integer.sign
                                    , n.integer.digits
                                    , "."
                                    , Maybe.withDefault "0" n.frac
                                    , "E"
                                    , rawToStr (Maybe.withDefault { sign = "+", digits = "0" } n.exp)
                                    ]

                ( _, _, Just "M" ) ->
                    P.succeed <|
                        BigFloat n.integer
                            (Maybe.withDefault "0" n.frac)
                            (Maybe.withDefault { sign = "+", digits = "0" } n.exp)

                _ ->
                    P.fail "mix of integer and floating point"
    in
    rawNumber
        |> P.andThen f
