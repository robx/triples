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


type Action
    = ActionSet
    | ActionNoSet


type Res
    = Correct
    | Wrong
    | Late


type UserEvent
    = Claimed
        { result : Result
        , action : Action
        , name : String
        }
    | Join String


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
    Html.div []
        [ Html.map User <|
            Play.viewGame
                { style = style
                , game = model.game
                , selected = model.selected
                , disableMore = False
                , answer = model.answer
                }
        , viewLog model.log
        ]


viewLog : List UserEvent -> Html.Html msg
viewLog events =
    let
        viewEvent e =
            case e of
                Join n ->
                    Html.p [] [ Html.text <| n ++ " joined!" ]

                _ ->
                    Html.p [] [ Html.text "unknown event" ]
    in
    Html.div [] <| List.map viewEvent events


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

        User Play.DealMore ->
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
                    Debug.crash "unknown change"

        Proto.Event event ->
            case event.eventOneof of
                Proto.Join join ->
                    { model | log = Join join.name :: model.log }

                Proto.Claimed claimed ->
                    model

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
