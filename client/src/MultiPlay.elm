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
import List.Extra
import Play
import Proto.Triples as Proto
import WebSocket


type Claim
    = ClaimSet (List Card.Card)
    | ClaimNoSet (List Card.Card)


type Action
    = ActionSet
    | ActionNoSet


type Res
    = Correct
    | Wrong
    | Late


type alias UserEvent =
    { result : Result
    , action : Action
    , name : String
    }


type alias Model =
    { wsURL : String
    , game : Game.GameView
    , selected : List Game.Pos
    , answer : Maybe Int
    , log : List UserEvent
    }


init : Game.GameView -> String -> Model
init game wsURL =
    { wsURL = wsURL
    , game = game
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
        Play.viewGame style model.game model.selected False model.answer


update : Msg -> Model -> ( Model, Maybe Claim )
update msg model =
    case msg of
        User (Play.Choose p) ->
            if Game.viewPosEmpty model.game p then
                ( model, Nothing )
            else if List.member p model.selected then
                ( { model | selected = List.Extra.remove p model.selected }, Nothing )
            else if List.length model.selected < (model.game.matchSize - 1) then
                ( { model | selected = p :: model.selected }, Nothing )
            else
                let
                    claimed =
                        p :: model.selected
                in
                ( { model | selected = [] }
                , Just <| ClaimSet <| List.filterMap (flip Dict.get model.game.table) <| claimed
                )

        User Play.DealMore ->
            ( model, Just <| ClaimNoSet <| Dict.values model.game.table )

        WSUpdate u ->
            case Decode.decodeString Proto.updateDecoder u of
                Err e ->
                    ( model, Nothing )

                Ok upd ->
                    ( applyUpdate upd model, Nothing )


applyUpdate : Proto.Update -> Model -> Model
applyUpdate update model =
    case update.updateOneof of
        Proto.Change change ->
            case change.changeOneof of
                Proto.Deal deal ->
                    model

                Proto.Match match ->
                    model

                Proto.Move move ->
                    model

                _ ->
                    model

        Proto.Event event ->
            case event.eventOneof of
                Proto.Join join ->
                    model

                Proto.Claimed claimed ->
                    model

                _ ->
                    model

        _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.wsURL WSUpdate
