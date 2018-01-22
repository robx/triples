module MultiPlay
    exposing
        ( Model
        , Msg(..)
        , init
        , subscriptions
        , update
        , view
        )

import Card
import Decode
import Dict
import Encode
import Game
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import List.Extra
import Parser
import Play
import WebSocket


type alias Model =
    { wsURL : String
    , game : Game.GameView
    , scores : List ( String, Int )
    , selected : List Game.Pos
    , answer : Maybe Int
    , log : List String
    }


init : Game.GameView -> String -> Model
init game wsURL =
    { wsURL = wsURL
    , game = game
    , scores = []
    , selected = []
    , answer = Nothing
    , log = []
    }


type Msg
    = User Play.UserMsg
    | WSUpdate String


view : Style.Style -> Model -> Html.Html Msg
view style model =
    Html.map User <|
        Play.viewGame
            { style = style
            , game = model.game
            , selected = model.selected
            , disableMore = False
            , answer = model.answer
            , info = Just { scores = model.scores, events = model.log }
            }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        User (Play.Choose p) ->
            if Game.viewPosEmpty model.game p then
                ( model, Cmd.none )
            else if List.member p model.selected then
                ( { model | selected = List.Extra.remove p model.selected }, Cmd.none )
            else if List.length model.selected < (model.game.matchSize - 1) then
                ( { model | selected = p :: model.selected }, Cmd.none )
            else
                let
                    claimed =
                        p :: model.selected
                in
                ( { model | selected = [] }
                , claim model.wsURL True <| List.filterMap (flip Dict.get model.game.table) <| claimed
                )

        User Play.UserDeal ->
            ( model
            , claim model.wsURL False <| Dict.values model.game.table
            )

        WSUpdate u ->
            case
                Decode.decodeString updateDecoder u
            of
                Err e ->
                    Debug.crash <| toString e

                Ok upd ->
                    ( applyUpdate upd model, Cmd.none )


type Update
    = Full FullRecord
    | EventJoin String
    | EventClaimed ClaimRecord
    | Change Game.Action


type alias Score =
    { match : Int
    , matchWrong : Int
    , noMatch : Int
    , noMatchWrong : Int
    }


type ClaimType
    = ClaimMatch
    | ClaimNoMatch


type ResultType
    = ResultCorrect
    | ResultWrong
    | ResultLate


type alias ClaimRecord =
    { name : String
    , type_ : ClaimType
    , result : ResultType
    , score : Score
    }


type alias FullRecord =
    { cols : Int
    , rows : Int
    , matchSize : Int
    , deckSize : Int
    , cards : Dict.Dict Game.Pos Card.Card
    , scores : Dict.Dict String Score
    }


updateDecoder : Decode.Decoder Update
updateDecoder =
    let
        pos =
            Decode.map2
                (,)
                (Decode.field "x" Decode.int)
                (Decode.field "y" Decode.int)

        card =
            Decode.map Card.fromInt Decode.int

        placedCard =
            Decode.map2
                (,)
                (Decode.field "position" pos)
                (Decode.field "card" card)

        cards =
            Decode.dict pos card

        score =
            Decode.map4
                Score
                (Decode.field "match" Decode.int)
                (Decode.field "matchWrong" Decode.int)
                (Decode.field "noMatch" Decode.int)
                (Decode.field "noMatchWrong" Decode.int)

        full =
            Decode.map Full <|
                Decode.map6
                    FullRecord
                    (Decode.field "cols" Decode.int)
                    (Decode.field "rows" Decode.int)
                    (Decode.field "matchSize" Decode.int)
                    (Decode.field "deckSize" Decode.int)
                    (Decode.field "cards" cards)
                    (Decode.field "scores" (Decode.dict Decode.string score))

        eventJoin =
            Decode.map EventJoin
                (Decode.field "name" Decode.string)

        claimType =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "match" ->
                                Decode.succeed ClaimMatch

                            "nomatch" ->
                                Decode.succeed ClaimNoMatch

                            _ ->
                                Decode.fail <| "unknown claim type: " ++ s
                    )

        resultType =
            Decode.string
                |> Decode.andThen
                    (\s ->
                        case s of
                            "correct" ->
                                Decode.succeed ResultCorrect

                            "wrong" ->
                                Decode.succeed ResultWrong

                            "late" ->
                                Decode.succeed ResultLate

                            _ ->
                                Decode.fail <| "unknown result type: " ++ s
                    )

        eventClaimed =
            Decode.map EventClaimed <|
                Decode.map4
                    ClaimRecord
                    (Decode.field "name" Decode.string)
                    (Decode.field "type" claimType)
                    (Decode.field "result" resultType)
                    (Decode.field "score" score)

        changeMatch =
            Decode.map (Change << Game.Match)
                (Decode.list pos)

        changeDeal =
            Decode.map (Change << Game.Deal)
                (Decode.list placedCard)

        move =
            Decode.map2
                (,)
                (Decode.field "from" pos)
                (Decode.field "to" pos)

        changeMove =
            Decode.map (Change << Game.Move)
                (Decode.list move)
    in
    Decode.tagged
        [ ( "triples/full", full )
        , ( "triples/eventJoin", eventJoin )
        , ( "triples/eventClaimed", eventClaimed )
        , ( "triples/changeMatch", changeMatch )
        , ( "triples/changeDeal", changeDeal )
        , ( "triples/changeMove", changeMove )
        ]


applyUpdate : Update -> Model -> Model
applyUpdate update model =
    let
        calcScore s =
            s.match - s.matchWrong + s.noMatch - s.noMatchWrong
    in
    case update of
        Full full ->
            { model
                | game =
                    { mincols = full.cols
                    , rows = full.rows
                    , table = full.cards
                    , deckSize = full.deckSize
                    , matchSize = full.matchSize
                    }
                , scores = Dict.toList (Dict.map (always calcScore) full.scores)
            }

        Change action ->
            { model | game = Game.viewApply action model.game }

        EventJoin name ->
            { model
                | log = (name ++ " joined!") :: model.log
                , scores = addPlayer name model.scores
            }

        EventClaimed claimed ->
            let
                typ =
                    case claimed.type_ of
                        ClaimMatch ->
                            "a triple"

                        ClaimNoMatch ->
                            "no triple"

                res =
                    case claimed.result of
                        ResultCorrect ->
                            "correct"

                        ResultWrong ->
                            "wrong"

                        ResultLate ->
                            "late"
            in
            { model
                | log = (claimed.name ++ " claimed " ++ typ ++ " (" ++ res ++ ")") :: model.log
                , scores = updateScores ( claimed.name, calcScore claimed.score ) model.scores
            }


updateScores : ( String, Int ) -> List ( String, Int ) -> List ( String, Int )
updateScores ( n, s ) scores =
    Dict.fromList scores |> Dict.insert n s |> Dict.toList |> List.sortBy (\( n, s ) -> ( s, n )) |> List.reverse


addPlayer : String -> List ( String, Int ) -> List ( String, Int )
addPlayer n scores =
    let
        add v =
            Just <| Maybe.withDefault 0 v
    in
    Dict.fromList scores |> Dict.update n add |> Dict.toList |> List.sortBy (\( n, s ) -> ( s, n )) |> List.reverse


type alias Claim =
    { type_ : ClaimType
    , cards : List Card.Card
    }


encodeClaim : Claim -> Encode.Element
encodeClaim claim =
    let
        claimType ct =
            case ct of
                ClaimMatch ->
                    Encode.string "match"

                ClaimNoMatch ->
                    Encode.string "nomatch"

        card c =
            Encode.int <| Card.toInt c
    in
    Encode.tag "triples/claim" <|
        Encode.object
            [ ( "type", claimType claim.type_ )
            , ( "cards", Encode.list <| List.map card claim.cards )
            ]


claim : String -> Bool -> List Card.Card -> Cmd msg
claim wsUrl match cards =
    WebSocket.send wsUrl <|
        Encode.encode <|
            encodeClaim <|
                { type_ =
                    if match then
                        ClaimMatch
                    else
                        ClaimNoMatch
                , cards = cards
                }


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.wsURL WSUpdate
