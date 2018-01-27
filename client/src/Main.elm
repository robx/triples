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


score : Game.GameDef -> List { time : Time.Time, event : Play.Event } -> { points : Int, message : String }
score def log =
    let
        end =
            Maybe.withDefault 0 <| Maybe.map .time <| List.head <| log

        start =
            Maybe.withDefault 0 <| Maybe.map .time <| List.head <| List.reverse <| log

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
            List.length <| List.filter (.event >> (==) Play.EDealMoreNonzero) <| log

        gooddeals =
            List.length <| List.filter (.event >> (==) Play.EDealMoreZero) <| log

        baddealsecs =
            baddeals * 60

        gooddealsecs =
            gooddeals * 45

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
