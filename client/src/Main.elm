port module Main exposing (..)

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
import Task
import Time
import UrlParser exposing ((<?>))


port getSize : () -> Cmd msg


type alias Size =
    { w : Int
    , h : Int
    }


port size : (Size -> msg) -> Sub msg


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
    , size : Size
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
      , size = { w = 0, h = 0 }
      }
    , getSize ()
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
    let
        maxSize =
            { w = min model.size.w 800, h = model.size.h }
    in
    Html.div [ HtmlA.id "container", HtmlA.style [ ( "background", model.params.style.colors.table ) ] ]
        [ Html.div
            [ HtmlA.id "main"
            , HtmlA.style [ ( "width", toString maxSize.w ++ "px" ), ( "height", toString maxSize.h ++ "px" ) ]
            ]
            [ case model.page of
                Menu menu ->
                    Menu.view Go menu

                Play game ->
                    Html.map (\m -> GetTimeAndThen (PlayMsg m)) <| Play.view model.params.style maxSize game

                MultiPlay game ->
                    Html.map MultiPlayMsg <| MultiPlay.view model.params.style maxSize game
            ]
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
    | NewGame Game.GameDef Game.Game
    | GetTimeAndThen (Time.Time -> Msg)
    | PlayMsg Play.Msg Time.Time
    | MultiPlayMsg MultiPlay.Msg
    | Ignore
    | APIResult (Result Http.Error String)
    | Resize Size


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

        ( Resize size, _ ) ->
            ( { model | size = size }, Cmd.none )

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
                ( model, Cmd.batch [ Random.generate (NewGame def) (Game.init def), Task.perform (PlayMsg Play.StartGame) Time.now ] )

        ( NewGame def game, _ ) ->
            ( { model | page = Play (Play.init def game) }, Cmd.none )

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
                        sc =
                            score pmodel.def log

                        scored =
                            model.params.telegramGame /= Nothing

                        send =
                            if scored then
                                case model.params.key of
                                    Just k ->
                                        sendScore model.location k sc.points

                                    _ ->
                                        Cmd.none
                            else
                                Cmd.none
                    in
                    ( { model | page = newMenu model.params (Just sc.message) }, send )

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

        parseParams parser location =
            UrlParser.parseHash parser { location | hash = "" }

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
    case parseParams (UrlParser.map f parser) loc of
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


type alias MatchStats =
    { time : Time.Time
    , event : Play.Event -- EMatch or EEnd
    , matchWrong : Int
    , noMatch : Int
    , noMatchWrong : Int
    }


type alias Stats =
    { start : Time.Time
    , end : Time.Time
    , total : Time.Time
    , match : Int
    , matchWrong : Int
    , noMatch : Int
    , noMatchWrong : Int
    , noMatchTime : Time.Time
    , matches : List MatchStats
    }


analyze : List Play.LogEntry -> Stats
analyze log =
    let
        filter e l =
            List.map .time <| List.filter ((==) e << .event) <| l

        one e l =
            Maybe.withDefault 0 <| List.head <| filter e l

        count e l =
            List.length <| filter e l

        start =
            one Play.EStart log

        end =
            one Play.EEnd log

        deltas l =
            List.map2 (\e1 e2 -> { time = e2.time - e1.time, event = e2.event }) l (List.drop 1 l)

        uptos p l =
            let
                ( prefix, rest ) =
                    List.Extra.break (not << p) l
            in
            case List.Extra.uncons rest of
                Just ( v, vs ) ->
                    (prefix ++ [ v ]) :: uptos p vs

                Nothing ->
                    []
    in
    { start = start
    , end = end
    , total = end - start
    , match = count Play.EMatch log
    , matchWrong = count Play.EMatchWrong log
    , noMatch = count Play.ENoMatch log
    , noMatchWrong = count Play.ENoMatchWrong log
    , noMatchTime =
        log
            |> List.filter (\e -> e.event == Play.EStart || e.event == Play.EMatch || e.event == Play.ENoMatch)
            |> deltas
            |> List.filter (\e -> e.event == Play.ENoMatch)
            |> List.map .time
            |> List.sum
    , matches =
        log
            |> deltas
            |> uptos (\e -> e.event == Play.EMatch || e.event == Play.EEnd)
            |> List.map
                (\es ->
                    { time = es |> List.map .time |> List.sum
                    , event = es |> List.reverse |> List.head |> Maybe.map .event |> Maybe.withDefault Play.EMatch
                    , matchWrong = count Play.EMatchWrong es
                    , noMatch = count Play.ENoMatch es
                    , noMatchWrong = count Play.ENoMatchWrong es
                    }
                )
    }


score : Game.GameDef -> List Play.LogEntry -> { points : Int, message : String }
score def log =
    let
        stats =
            analyze log

        secs =
            round <| Time.inSeconds stats.total

        format secs =
            let
                m =
                    secs // 60

                s =
                    secs % 60
            in
            toString m ++ ":" ++ (String.padLeft 2 '0' <| toString s)

        baddealsecs =
            stats.noMatchWrong * 60

        gooddealsecs =
            stats.noMatch * 45

        totalsecs =
            secs + baddealsecs - gooddealsecs

        pts =
            10000 // totalsecs
    in
    { points = pts
    , message = String.join " " [ "Your time:", format totalsecs, "=", format secs, "+", format baddealsecs, "-", format gooddealsecs, "(" ++ toString pts ++ ")" ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ size Resize
        , case model.page of
            MultiPlay m ->
                Sub.map MultiPlayMsg <| MultiPlay.subscriptions m

            _ ->
                Sub.none
        ]
