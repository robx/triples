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
import Dict
import Game
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
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
                Decode.decodeString Proto.updateDecoder u
            of
                Err e ->
                    Debug.crash e

                Ok upd ->
                    ( applyUpdate upd model, Cmd.none )


applyUpdate : Proto.Update -> Model -> Model
applyUpdate update model =
    let
        getScore s =
            case s of
                Nothing ->
                    0

                Just ss ->
                    ss.match - ss.matchwrong + ss.nomatch - ss.nomatchwrong

        toScore ps =
            ( ps.name, getScore ps.score )

        getPosition p =
            case p of
                Nothing ->
                    ( 0, 0 )

                Just pp ->
                    ( pp.x, pp.y )

        toList pcs =
            List.map (\pc -> ( getPosition pc.position, Card.fromInt pc.card )) <|
                pcs

        toDict pcs =
            Dict.fromList <| toList pcs
    in
    case update.updateOneof of
        Proto.Full full ->
            { model
                | game =
                    { cols = full.cols
                    , rows = full.rows
                    , table = toDict full.cards
                    , deckSize = full.deckSize
                    , matchSize = full.matchSize
                    }
                , scores = List.map toScore full.scores
            }

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
                Proto.Join join ->
                    { model | log = (join.name ++ " joined!") :: model.log }

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
                    { model | log = (claimed.name ++ " claimed " ++ typ ++ " (" ++ res ++ ")") :: model.log }

                _ ->
                    Debug.crash "unknown event"

        _ ->
            Debug.crash "unknown update"


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
