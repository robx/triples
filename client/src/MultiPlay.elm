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
    , scores : Dict.Dict String Status
    , selected : List Game.Pos
    , log : List String
    }


init : Game.GameView -> String -> Model
init game wsURL =
    { wsURL = wsURL
    , game = game
    , scores = Dict.empty
    , selected = []
    , log = []
    }


type UserMsg
    = Choose Game.Pos
    | UserDeal
    | UserStart


type Msg
    = User UserMsg
    | WSUpdate String


view : Style.Style -> Play.Size -> Model -> Html.Html Msg
view style maxSize model =
    Html.map User <|
        Play.viewGame
            { style = style
            , maxSize = maxSize
            , game = model.game
            , selected = model.selected
            , button =
                { message =
                    if Dict.size model.game.table == 0 then
                        Just UserStart
                    else
                        Just UserDeal
                , label =
                    if Dict.size model.game.table == 0 then
                        ">"
                    else if model.game.deckSize == 0 then
                        "."
                    else
                        "+"
                }
            , choose = Choose
            , info = Just { scores = scoreTable model.scores, events = model.log }
            }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        User (Choose p) ->
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
                , sendCommand model.wsURL <|
                    Claim
                        ClaimMatch
                        (List.filterMap (flip Dict.get model.game.table) claimed)
                )

        User UserDeal ->
            ( model
            , sendCommand model.wsURL <|
                Claim
                    ClaimNoMatch
                    (Dict.values model.game.table)
            )

        User UserStart ->
            ( model
            , sendCommand model.wsURL <| Start
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
    | EventOnline String Bool
    | EventClaimed ClaimRecord
    | Change Game.Action


type alias Status =
    { present : Bool
    , score : Int
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
    , score : Int
    }


type alias FullRecord =
    { cols : Int
    , rows : Int
    , matchSize : Int
    , deckSize : Int
    , cards : Dict.Dict Game.Pos Card.Card
    , players : Dict.Dict String Status
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

        status =
            Decode.map2
                Status
                (Decode.field "present" Decode.bool)
                (Decode.field "score" Decode.int)

        full =
            Decode.map Full <|
                Decode.map6
                    FullRecord
                    (Decode.field "cols" Decode.int)
                    (Decode.field "rows" Decode.int)
                    (Decode.field "matchSize" Decode.int)
                    (Decode.field "deckSize" Decode.int)
                    (Decode.field "cards" cards)
                    (Decode.field "players" (Decode.dict Decode.string status))

        eventOnline =
            Decode.map2 EventOnline
                (Decode.field "name" Decode.string)
                (Decode.field "present" Decode.bool)

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
                    (Decode.field "score" Decode.int)

        changeMatch =
            Decode.map (Change << Game.Match)
                (Decode.anyList pos)

        changeDeal =
            Decode.map (Change << Game.Deal)
                (Decode.anyList placedCard)

        move =
            Decode.map2
                (,)
                (Decode.field "from" pos)
                (Decode.field "to" pos)

        changeMove =
            Decode.map (Change << Game.Move)
                (Decode.anyList move)
    in
    Decode.tagged
        [ ( "triples/full", full )
        , ( "triples/eventOnline", eventOnline )
        , ( "triples/eventClaimed", eventClaimed )
        , ( "triples/changeMatch", changeMatch )
        , ( "triples/changeDeal", changeDeal )
        , ( "triples/changeMove", changeMove )
        ]


applyUpdate : Update -> Model -> Model
applyUpdate update model =
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
                , scores = full.players
                , selected = []
            }

        Change action ->
            { model | game = Game.viewApply action model.game, selected = Game.selectedApply action model.selected }

        EventOnline name online ->
            { model
                | log =
                    (name
                        ++ (if online then
                                " connected"
                            else
                                " disconnected"
                           )
                    )
                        :: model.log
                , scores = updateStatus name (\s -> { s | present = online }) model.scores
            }

        EventClaimed claimed ->
            let
                typ =
                    case claimed.type_ of
                        ClaimMatch ->
                            "a match"

                        ClaimNoMatch ->
                            "no match"

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
                , scores = updateStatus claimed.name (\s -> { s | score = claimed.score }) model.scores
            }


updateStatus : String -> (Status -> Status) -> Dict.Dict String Status -> Dict.Dict String Status
updateStatus name f =
    Dict.update
        name
        (\ms -> ms |> Maybe.withDefault { score = 0, present = False } |> f |> Just)


scoreTable : Dict.Dict String Status -> List ( String, Status )
scoreTable statuses =
    statuses
        |> Dict.toList
        |> List.sortBy
            (\( n, s ) ->
                ( -s.score
                , if s.present then
                    0
                  else
                    1
                , n
                )
            )


type Command
    = Claim ClaimType (List Card.Card)
    | Start


encodeCommand : Command -> Encode.Element
encodeCommand cmd =
    case cmd of
        Claim ct cs ->
            let
                claimType =
                    case ct of
                        ClaimMatch ->
                            Encode.string "match"

                        ClaimNoMatch ->
                            Encode.string "nomatch"

                card c =
                    Encode.int <| Card.toInt c
            in
            Encode.mustTag "triples/claim" <|
                Encode.mustObject
                    [ ( "type", claimType )
                    , ( "cards", Encode.list <| List.map card cs )
                    ]

        Start ->
            Encode.mustTag "triples/start" (Encode.object [])


sendCommand : String -> Command -> Cmd msg
sendCommand wsUrl =
    WebSocket.send wsUrl << Encode.encode << encodeCommand


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.wsURL WSUpdate
