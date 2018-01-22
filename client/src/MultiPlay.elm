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
import Game
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Json.Encode as Encode
import List.Extra
import Parser
import Play
import Proto.Triples as Proto
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


type alias Score =
    { match : Int
    , matchWrong : Int
    , noMatch : Int
    , noMatchWrong : Int
    }


type Update
    = Full FullRecord
    | EventJoin String



{- record alias for Decode.map -}


type alias FullRecord =
    { cols : Int
    , rows : Int
    , matchSize : Int
    , deckSize : Int
    , cards : Dict.Dict Game.Pos Card.Card
    , scores : Dict.Dict String Score
    }


posDecoder : Decode.Decoder Game.Pos
posDecoder =
    Decode.map2
        (,)
        (Decode.field "x" Decode.int)
        (Decode.field "y" Decode.int)


cardDecoder : Decode.Decoder Card.Card
cardDecoder =
    Decode.map Card.fromInt Decode.int


scoreDecoder : Decode.Decoder Score
scoreDecoder =
    Decode.map4
        Score
        (Decode.field "match" Decode.int)
        (Decode.field "matchWrong" Decode.int)
        (Decode.field "noMatch" Decode.int)
        (Decode.field "noMatchWrong" Decode.int)


updateDecoder : Decode.Decoder Update
updateDecoder =
    let
        fullDecoder =
            Decode.map6
                FullRecord
                (Decode.field "cols" Decode.int)
                (Decode.field "rows" Decode.int)
                (Decode.field "matchSize" Decode.int)
                (Decode.field "deckSize" Decode.int)
                (Decode.field "cards" (Decode.dict posDecoder cardDecoder))
                (Decode.field "scores" (Decode.dict Decode.string scoreDecoder))

        eventJoinDecoder =
            Decode.field "name" Decode.string
    in
    Decode.tagged
        [ ( "triples/full", Decode.map Full fullDecoder )
        , ( "triples/eventJoin", Decode.map EventJoin eventJoinDecoder )
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

        EventJoin name ->
            { model
                | log = (name ++ " joined!") :: model.log
                , scores = addPlayer name model.scores
            }



{-
   Proto.Change change ->
       let
           action =
               case change.changeOneof of
                   Proto.Deal deal ->
                       Game.Deal <| toList deal.cards

                   Proto.Match match ->
                       Game.Match <| List.map (getPosition << Just) match.positions

                   Proto.Move move ->
                       Game.Move <| List.map (\m -> ( getPosition m.from, getPosition m.to )) move.moves

                   _ ->
                       Debug.crash "unknown change"
       in
       { model | game = Game.viewApply action model.game }

   Proto.Event event ->
       case event.eventOneof of
           Proto.Claimed claimed ->
               let
                   res =
                       case claimed.result of
                           Proto.UpdateEvent_EventClaimed_Correct ->
                               "correct"

                           Proto.UpdateEvent_EventClaimed_Wrong ->
                               "wrong"

                           Proto.UpdateEvent_EventClaimed_Late ->
                               "late"

                   typ =
                       case claimed.type_ of
                           Proto.ClaimMatch ->
                               "a triple"

                           Proto.ClaimNomatch ->
                               "no triple"
               in
               { model
                   | log = (claimed.name ++ " claimed " ++ typ ++ " (" ++ res ++ ")") :: model.log
                   , scores = updateScores (toScore claimed) model.scores
               }

           _ ->
               Debug.crash "unknown event"
-}


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


claim : String -> Bool -> List Card.Card -> Cmd msg
claim wsUrl match cards =
    WebSocket.send wsUrl <|
        (Encode.encode 0 << Proto.claimEncoder) <|
            { type_ =
                if match then
                    Proto.ClaimMatch
                else
                    Proto.ClaimNomatch
            , cards = List.map Card.toInt cards
            }


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.wsURL WSUpdate
