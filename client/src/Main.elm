module Main exposing (..)

import Card exposing (..)
import Debug
import Dict
import Game exposing (Game)
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Http
import List.Extra
import Menu
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
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { params : Params
    , page : Page
    }


type Page
    = Menu Menu.Model
    | Play Play.Model


type alias Params =
    { style : Style.Style
    , key : Maybe String
    , name : Maybe String
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    ( { params = parseParams loc
      , page = Menu Nothing
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    Html.div [ HtmlA.id "container", HtmlA.style [ ( "background", model.params.style.colors.table ) ] ]
        [ case model.page of
            Menu menu ->
                Menu.view Go model.params.name model.params.style menu

            Play game ->
                Html.map (\m -> GetTimeAndThen (PlayMsg m)) <| Play.view model.params.style game
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
    = Go Bool Bool
    | NewGame Game
    | GetTimeAndThen (Time.Time -> Msg)
    | PlayMsg Play.Msg Time.Time
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

        ( Go short super, _ ) ->
            ( model, Cmd.batch [ Random.generate NewGame (Game.init short super), Task.perform (PlayMsg Play.StartGame) Time.now ] )

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
                            case model.page of
                                Menu _ ->
                                    False

                                Play g ->
                                    g.game.type_ == Game.ClassicSet && not g.game.short

                        send =
                            if scored then
                                case model.params.key of
                                    Just k ->
                                        sendScore k telescore

                                    _ ->
                                        Cmd.none
                            else
                                Cmd.none
                    in
                    ( { model | page = Menu (Just msg) }, send )

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

        f s k n =
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
            }
    in
    case UrlParser.parseHash (UrlParser.map f parser) loc of
        Nothing ->
            Debug.crash "url parse failure"

        Just p ->
            p


sendScore : String -> Int -> Cmd Msg
sendScore key score =
    Http.send APIResult <|
        Http.getString <|
            "https://arp.vllmrt.net/triples/api/win?key="
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
