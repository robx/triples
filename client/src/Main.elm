module Main exposing (..)

import Card exposing (..)
import Debug
import Dict
import Game
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Http
import List.Extra
import Menu
import MultiPlay
import Navigation
import Play
import Process
import Random
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Task
import Time
import UrlParser exposing ((<?>))


main =
    Navigation.program (always Ignore)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { location : Navigation.Location
    , params : Params
    , page : Page
    }


type Page
    = Menu Menu.Model
    | Play Play.Model
    | MultiPlay MultiPlay.Model


type alias Params =
    { style : Style.Style
    , key : Maybe String
    , name : Maybe String
    , telegramGame : Maybe Game.GameDef
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    let
        params =
            parseParams loc
    in
    ( { location = loc
      , params = params
      , page = newMenu params Nothing
      }
    , Cmd.none
    )


newMenu : Params -> Maybe String -> Page
newMenu params msg =
    Menu
        { score = msg
        , name = params.name
        , telegramGame = params.telegramGame
        , style = params.style
        }


view : Model -> Html.Html Msg
view model =
    Html.div [ HtmlA.id "container", HtmlA.style [ ( "background", model.params.style.colors.table ) ] ]
        [ case model.page of
            Menu menu ->
                Menu.view Go menu

            Play game ->
                Html.map (\m -> GetTimeAndThen (PlayMsg m)) <| Play.view model.params.style game

            MultiPlay game ->
                Html.map MultiPlayMsg <| MultiPlay.view model.params.style game
        ]


after : Time.Time -> (Time.Time -> Msg) -> Cmd Msg
after time msg =
    let
        task =
            Time.now
                |> Task.andThen
                    (\now ->
                        Process.sleep time
                            |> Task.andThen (\_ -> Task.succeed now)
                    )
    in
    Task.perform msg task


type Msg
    = Go Game.GameDef
    | NewGame Game.Game
    | GetTimeAndThen (Time.Time -> Msg)
    | PlayMsg Play.Msg Time.Time
    | MultiPlayMsg MultiPlay.Msg
    | Ignore
    | APIResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( APIResult r, _ ) ->
            let
                _ =
                    Debug.log "api result" r
            in
            ( model, Cmd.none )

        ( Ignore, _ ) ->
            ( model, Cmd.none )

        ( GetTimeAndThen m, _ ) ->
            ( model, Task.perform m Time.now )

        ( Go def, _ ) ->
            if def.multi then
                let
                    k =
                        Maybe.withDefault "" model.params.key

                    m =
                        MultiPlay.init (Game.empty def) (joinUrl model.location ++ "?key=" ++ k)
                in
                ( { model | page = MultiPlay m }, Cmd.none )
            else
                ( model, Cmd.batch [ Random.generate NewGame (Game.init def), Task.perform (PlayMsg Play.StartGame) Time.now ] )

        ( NewGame game, _ ) ->
            ( { model | page = Play (Play.init game) }, Cmd.none )

        ( PlayMsg pmsg now, Play pmodel ) ->
            let
                ( newpmodel, res ) =
                    Play.update now pmsg pmodel
            in
            case res of
                Nothing ->
                    ( { model | page = Play newpmodel }, Cmd.none )

                Just (Play.After delay m) ->
                    ( { model | page = Play newpmodel }, after delay (PlayMsg m) )

                Just (Play.GameOver log) ->
                    let
                        ( secs, msg, telescore ) =
                            score log

                        scored =
                            model.params.telegramGame /= Nothing

                        send =
                            if scored then
                                case model.params.key of
                                    Just k ->
                                        sendScore model.location k telescore

                                    _ ->
                                        Cmd.none
                            else
                                Cmd.none
                    in
                    ( { model | page = newMenu model.params (Just msg) }, send )

        ( MultiPlayMsg pmsg, MultiPlay pmodel ) ->
            let
                ( newpmodel, cmd ) =
                    MultiPlay.update pmsg pmodel
            in
            ( { model | page = MultiPlay newpmodel }, cmd )

        _ ->
            ( model, Cmd.none )


parseParams : Navigation.Location -> Params
parseParams loc =
    let
        parser =
            UrlParser.top
                <?> UrlParser.stringParam "style"
                <?> UrlParser.stringParam "key"
                <?> UrlParser.stringParam "name"
                <?> UrlParser.stringParam "game"

        f s k n g =
            { style =
                case Maybe.withDefault "square" s of
                    "classic" ->
                        Style.classic

                    "modified" ->
                        Style.modified

                    _ ->
                        Style.square
            , key = k
            , name = n
            , telegramGame =
                case g of
                    Just "triples" ->
                        Just Game.defTriples

                    Just "quadruples" ->
                        Just Game.defQuadruples

                    Just "triplesmulti" ->
                        Just Game.defTriplesMulti

                    _ ->
                        Nothing
            }
    in
    case UrlParser.parseHash (UrlParser.map f parser) loc of
        Nothing ->
            Debug.crash "url parse failure"

        Just p ->
            p


(</>) : String -> String -> String
(</>) a b =
    case ( String.right 1 a, String.left 1 b ) of
        ( "/", "/" ) ->
            a ++ String.dropLeft 1 b

        ( _, "/" ) ->
            a ++ b

        ( "/", _ ) ->
            a ++ b

        _ ->
            a ++ "/" ++ b


winUrl : Navigation.Location -> String
winUrl loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname </> "api/win"


joinUrl : Navigation.Location -> String
joinUrl loc =
    let
        protocol =
            if loc.protocol == "https:" then
                "wss:"
            else
                "ws:"
    in
    protocol ++ "//" ++ loc.host ++ loc.pathname </> "api/join"


sendScore : Navigation.Location -> String -> Int -> Cmd Msg
sendScore location key score =
    Http.send APIResult <|
        Http.getString <|
            winUrl location
                ++ "?key="
                ++ key
                ++ "&score="
                ++ toString score


score : List ( Time.Time, Play.Event ) -> ( Int, String, Int )
score log =
    let
        end =
            Maybe.withDefault 0 <| Maybe.map Tuple.first <| List.head <| log

        start =
            Maybe.withDefault 0 <| Maybe.map Tuple.first <| List.head <| List.reverse <| log

        secs =
            round <| Time.inSeconds (end - start)

        format secs =
            let
                m =
                    secs // 60

                s =
                    secs % 60
            in
            toString m ++ ":" ++ (String.padLeft 2 '0' <| toString s)

        baddeals =
            List.length <| List.filter (Tuple.second >> (==) Play.EDealMoreNonzero) <| log

        gooddeals =
            List.length <| List.filter (Tuple.second >> (==) Play.EDealMoreZero) <| log

        baddealsecs =
            baddeals * 60

        gooddealsecs =
            gooddeals * 45

        totalsecs =
            secs + baddealsecs - gooddealsecs
    in
    ( totalsecs
    , String.join " " [ "Your time:", format totalsecs, "=", format secs, "+", format baddealsecs, "-", format gooddealsecs ]
    , 10000 // totalsecs
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        MultiPlay m ->
            Sub.map MultiPlayMsg <| MultiPlay.subscriptions m

        _ ->
            Sub.none
