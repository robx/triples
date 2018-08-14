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
    | Waiting
    | Play Play.Model
    | MultiPlay MultiPlay.Model


type alias Params =
    { style : Style.Style
    , key : Maybe String
    , room : Maybe String
    , name : Maybe String
    , game : Maybe Game.GameDef
    , scored : Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    let
        params =
            parseParams loc

        genRoom =
            case params.room of
                Nothing ->
                    [ Random.generate SetRoom (randHex 6) ]

                Just _ ->
                    []
    in
    ( { location = loc
      , params = params
      , page = newMenu params Nothing
      , size = { w = 0, h = 0 }
      }
    , Cmd.batch <| getSize () :: genRoom
    )


newMenu : Params -> Maybe Menu.Score -> Page
newMenu params score =
    Menu
        { score = score
        , scoreDetails = False
        , name = params.name
        , game = params.game
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
                    Menu.view MenuMsg Go menu

                Waiting ->
                    Html.div [] [ Html.text "waiting" ]

                Play game ->
                    Html.map (\m -> GetTimeAndThen (PlayMsg m)) <| Play.view model.params.style maxSize game

                MultiPlay game ->
                    Html.map MultiPlayMsg <| MultiPlay.view model.params.style maxSize game
            ]
        ]


type Msg
    = Go Game.GameDef String
    | MenuMsg Menu.Msg
    | NewGame Game.GameDef Game.Game
    | GetTimeAndThen (Time.Time -> Msg)
    | PlayMsg Play.Msg Time.Time
    | MultiPlayMsg MultiPlay.Msg
    | Ignore
    | APIWinResult (Result Http.Error String)
    | APINewResult Msg (Result Http.Error String)
    | Resize Size
    | SetRoom String


type APICall
    = APIWin
    | APINew


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( APIWinResult r, _ ) ->
            let
                _ =
                    Debug.log "api result (win)" r
            in
            ( model, Cmd.none )

        ( Ignore, _ ) ->
            ( model, Cmd.none )

        ( GetTimeAndThen m, _ ) ->
            ( model, Task.perform m Time.now )

        ( Resize size, _ ) ->
            ( { model | size = size }, Cmd.none )

        ( MenuMsg mmsg, Menu menu ) ->
            ( { model | page = Menu (Menu.update mmsg menu) }, Cmd.none )

        ( Go def name, _ ) ->
            if def.multi then
                let
                    room =
                        Maybe.withDefault "veryunlikely" model.params.room

                    game =
                        Game.gameId def

                    ws =
                        joinUrl model.location ++ "?room=" ++ room ++ "&game=" ++ game ++ "&name=" ++ name

                    share =
                        shareUrl model.location ++ "?room=" ++ room ++ "&game=" ++ game

                    m =
                        MultiPlay.init (Game.empty def) ws share
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

                Just (Play.Command cmd) ->
                    ( { model | page = Play newpmodel }, Cmd.map (GetTimeAndThen << PlayMsg) cmd )

                Just (Play.GameOver log) ->
                    let
                        sc =
                            score pmodel.def log

                        send =
                            if model.params.scored then
                                case model.params.key of
                                    Just k ->
                                        sendScore model.location k sc.points

                                    _ ->
                                        Cmd.none

                            else
                                Cmd.none
                    in
                    ( { model | page = newMenu model.params (Just sc.messages) }, send )

        ( MultiPlayMsg pmsg, MultiPlay pmodel ) ->
            let
                ( newpmodel, cmd ) =
                    MultiPlay.update pmsg pmodel
            in
            ( { model | page = MultiPlay newpmodel }, cmd )

        ( SetRoom room, _ ) ->
            let
                oldParams =
                    model.params
            in
            ( { model | params = { oldParams | room = Just room } }, Cmd.none )

        _ ->
            ( model, Cmd.none )


parseParams : Navigation.Location -> Params
parseParams loc =
    let
        parser =
            UrlParser.top
                <?> UrlParser.stringParam "style"
                <?> UrlParser.stringParam "key"
                <?> UrlParser.stringParam "room"
                <?> UrlParser.stringParam "name"
                <?> UrlParser.stringParam "game"
                <?> UrlParser.stringParam "scored"

        parseParams parser location =
            UrlParser.parseHash parser { location | hash = "" }

        f s k r n g sc =
            { style =
                case Maybe.withDefault "square" s of
                    "classic" ->
                        Style.classic

                    "modified" ->
                        Style.modified

                    _ ->
                        Style.square
            , key = k
            , room = r
            , name = n
            , game =
                case g of
                    Just gg ->
                        Game.lookupDef gg

                    Nothing ->
                        Nothing
            , scored = sc == Just "1"
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


shareUrl : Navigation.Location -> String
shareUrl loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname


winUrl : Navigation.Location -> String
winUrl loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname </> "api/win"


newUrl : Navigation.Location -> String
newUrl loc =
    loc.protocol ++ "//" ++ loc.host ++ loc.pathname </> "api/new"


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
    Http.send APIWinResult <|
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
    , hint : Int
    , noMatchTime : Time.Time
    , matches : List MatchStats
    }


analyze : List Play.LogEntry -> Stats
analyze rlog =
    let
        log =
            List.reverse rlog

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
    in
    { start = start
    , end = end
    , total = end - start
    , match = count Play.EMatch log
    , matchWrong = count Play.EMatchWrong log
    , noMatch = count Play.ENoMatch log
    , noMatchWrong = count Play.ENoMatchWrong log
    , hint = count Play.EHint log
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


uptos : (a -> Bool) -> List a -> List (List a)
uptos p l =
    let
        ( prefix, rest ) =
            List.Extra.break p l
    in
    case List.Extra.uncons rest of
        Just ( v, vs ) ->
            (prefix ++ [ v ]) :: uptos p vs

        Nothing ->
            []


stat : List Float -> { average : Float, median : Float, max : Float, min : Float }
stat vs =
    let
        n =
            List.length vs
    in
    { average = List.sum vs / toFloat n
    , max = List.maximum vs |> Maybe.withDefault 0
    , min = List.minimum vs |> Maybe.withDefault 0
    , median =
        let
            sorted =
                List.sort vs
        in
        case n % 2 of
            1 ->
                case List.Extra.getAt (n // 2) sorted of
                    Just v ->
                        v

                    Nothing ->
                        0

            _ ->
                case ( List.Extra.getAt (n // 2 - 1) sorted, List.Extra.getAt (n // 2) sorted ) of
                    ( Just v1, Just v2 ) ->
                        (v1 + v2) / 2

                    _ ->
                        0
    }


score : Game.GameDef -> List Play.LogEntry -> { points : Int, messages : Menu.Score }
score def log =
    let
        stats =
            analyze log

        matchStats =
            stats.matches |> List.map (Time.inSeconds << .time) |> stat

        cards =
            .deckSize <| Game.empty def

        cpm =
            toFloat cards / Time.inMinutes stats.total

        discountCpm =
            toFloat cards / Time.inMinutes (stats.total - stats.noMatchTime)

        seconds =
            round << Time.inSeconds

        format secs =
            let
                m =
                    secs // 60

                s =
                    secs % 60
            in
            toString m ++ ":" ++ (String.padLeft 2 '0' <| toString s)

        fseconds =
            format << seconds

        mistakeFactor =
            0.9 ^ toFloat stats.noMatchWrong * 0.95 ^ toFloat stats.matchWrong * 0.95 ^ toFloat stats.hint

        pts =
            round <| 10 * discountCpm * mistakeFactor

        fsec s =
            toString (toFloat (round (s * 10)) / 10) ++ "s"

        ffloat n f =
            toString <| toFloat (round (f * 10 ^ n)) / 10 ^ n
    in
    { points = pts
    , messages =
        { summary = "Done! You won " ++ toString pts ++ " points in " ++ fseconds stats.total ++ " minutes."
        , details =
            [ String.join " "
                [ "Your total time:", fseconds stats.total, "(" ++ ffloat 1 cpm ++ " cards/minute)" ]
            , String.join " "
                [ "Effective time:", fseconds (stats.total - stats.noMatchTime), "(" ++ ffloat 1 discountCpm ++ " cards/minute)" ]
            , String.join " "
                [ "Deductions:", "match", toString stats.matchWrong, "no match", toString stats.noMatchWrong, "hint", toString stats.hint ]
            , String.join " "
                [ "Points", toString pts, "=", ffloat 2 mistakeFactor, "*", ffloat 0 (discountCpm * 10) ]
            , String.join " "
                [ "Match find stats:", "average", fsec matchStats.average, "median", fsec matchStats.median, "minimum", fsec matchStats.min, "maximum", fsec matchStats.max ]
            ]
        }
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



--| Generate n random bytes, hex encoded


randHex : Int -> Random.Generator String
randHex n =
    let
        hex i =
            case i of
                10 ->
                    "a"

                11 ->
                    "b"

                12 ->
                    "c"

                13 ->
                    "d"

                14 ->
                    "e"

                15 ->
                    "f"

                _ ->
                    toString i

        one =
            Random.int 0 15 |> Random.map hex
    in
    Random.list (2 * n) one |> Random.map String.concat
