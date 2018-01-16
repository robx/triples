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
import List.Extra
import Play
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
            else if List.length model.selected < (model.game.setSize - 1) then
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
            case parseUpdate u of
                Err e ->
                    ( model, Nothing )

                Ok a ->
                    ( applyAction a model, Nothing )


parseUpdate : String -> Result String Game.Action
parseUpdate s = Err "not implemented"


applyAction : Game.Action -> Model -> Model
applyAction action model =
    let
        moves =
            always Nothing
    in
    { model
        | game = Game.viewApply action model.game
        , selected = List.filterMap moves model.selected
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen model.wsURL WSUpdate
