module Main exposing (..)

import Card exposing (..)
import Debug
import Dict
import Game exposing (Game)
import Graphics
import Html
import Html.Attributes as Html
import Html.Events as Html
import Http
import List.Extra
import Navigation
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


type alias GameModel =
    { game : Game
    , start : Time.Time
    , log : List Event
    , selected : List Game.Pos
    , dealing : Bool
    , answer : Maybe Int
    }


initGame : Game -> GameModel
initGame game =
    { game = game
    , start = 0
    , log = []
    , selected = []
    , dealing = False
    , answer = Nothing
    }


type Event
    = ESet
    | EDealMoreZero
    | EDealMoreNonzero


type alias Model =
    { params : Params
    , page : Page
    }


type Page
    = Start (Maybe String)
    | Play GameModel


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
    ( { params = parseParams loc
      , page = Start Nothing
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    Html.div [ Html.id "container", Html.style [ ( "background", model.params.style.colors.table ) ] ]
        [ case model.page of
            Start msg ->
                viewStart model.params.name model.params.style msg

            Play game ->
                viewGame model.params.style game
        ]


viewGame : Graphics.Style -> GameModel -> Html.Html Msg
viewGame style model =
    let
        d ( pos, card ) =
            Svg.g
                [ SvgA.transform (trans pos)
                , SvgE.onClick (Choose pos)
                , SvgA.style "cursor: pointer;"
                ]
                [ Graphics.draw style (List.member pos model.selected) card ]

        gs =
            Dict.toList model.game.table |> List.map d

        cols =
            Game.columns model.game

        more =
            let
                text =
                    case model.answer of
                        Just n ->
                            toString n

                        Nothing ->
                            if Game.deckEmpty model.game then
                                "."
                            else
                                "+"

                disabled =
                    model.dealing
                        || model.answer
                        /= Nothing

                handler =
                    if disabled then
                        []
                    else
                        [ SvgE.onClick DealMore
                        , SvgA.style "cursor: pointer;"
                        ]
            in
            Svg.g
                (SvgA.transform (trans ( cols, 0 )) :: handler)
                [ Graphics.button style text ]

        trans ( c, r ) =
            let
                x =
                    (toFloat c + 0.5) * (10 + style.layout.w)

                y =
                    (toFloat r + 0.5) * (10 + style.layout.h)
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        viewBox =
            let
                width =
                    (style.layout.w + 10) * (toFloat cols + 1)

                height =
                    (style.layout.h + 10) * 3
            in
            "0 0 " ++ toString width ++ " " ++ toString height
    in
    Svg.svg
        [ SvgA.viewBox viewBox
        , Html.id "main"
        , Html.style [ ( "background", style.colors.table ) ]
        ]
        (Graphics.svgDefs style :: more :: gs)


viewStart : Maybe String -> Graphics.Style -> Maybe String -> Html.Html Msg
viewStart name style score =
    let
        addScore h =
            case score of
                Just m ->
                    Html.div [ Html.class "msg", Html.style [ ( "background", snd style.colors.symbols ) ] ] [ Html.text m ] :: h

                Nothing ->
                    h

        prompt =
            "Choose a game"
                ++ (case name of
                        Just n ->
                            ", " ++ n ++ "!"

                        Nothing ->
                            "!"
                   )

        fst ( x, y, z ) =
            x

        snd ( x, y, z ) =
            y

        trd ( x, y, z ) =
            z
    in
    Html.div [ Html.id "main" ] <|
        addScore
            [ Html.div
                [ Html.class "msg", Html.style [ ( "background", trd style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , Html.div [ Html.class "buttons" ]
                [ Html.button [ Html.onClick <| Go False False ] [ Html.text "Classic (scored!)" ]
                , Html.button [ Html.onClick <| Go True False ] [ Html.text "Classic (short)" ]
                , Html.button [ Html.onClick <| Go False True ] [ Html.text "Super" ]
                , Html.button [ Html.onClick <| Go True True ] [ Html.text "Super (short)" ]
                ]
            ]


type Msg
    = Go Bool Bool
    | NewGame Game
    | StartGame Time.Time
    | Choose Game.Pos
    | Deal
    | DealMore
    | GameOver (List Event) Time.Time Time.Time
    | Ignore
    | APIResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        APIResult r ->
            let
                _ =
                    Debug.log "api result" r
            in
            ( model, Cmd.none )

        Ignore ->
            ( model, Cmd.none )

        Go short super ->
            ( model, Cmd.batch [ Random.generate NewGame (Game.init short super), Task.perform StartGame Time.now ] )

        NewGame game ->
            ( { model | page = Play (initGame game) }, Cmd.none )

        GameOver log start now ->
            let
                ( secs, msg, telescore ) =
                    score log start now

                scored =
                    case model.page of
                        Start _ ->
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
            ( { model | page = Start (Just msg) }, send )

        _ ->
            case model.page of
                Play game ->
                    let
                        ( updgame, cmd ) =
                            updateGame msg game
                    in
                    ( { model | page = Play updgame }, cmd )

                _ ->
                    Debug.crash "bad state"


updateGame : Msg -> GameModel -> ( GameModel, Cmd Msg )
updateGame msg model =
    let
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
    in
    case msg of
        StartGame start ->
            ( { model | start = start }, Cmd.none )

        Choose p ->
            if Game.posEmpty model.game p then
                ( model, Cmd.none )
            else if List.member p model.selected then
                ( { model | selected = List.Extra.remove p model.selected }, Cmd.none )
            else if List.length model.selected < (Game.setSize model.game - 1) then
                ( { model | selected = p :: model.selected }, Cmd.none )
            else
                let
                    ( isset, newgame ) =
                        Game.take model.game (p :: model.selected)
                in
                if isset then
                    let
                        log =
                            ESet :: model.log
                    in
                    ( { model | game = newgame, selected = [], dealing = True, answer = Nothing, log = log }
                    , after 500 <| always Deal
                    )
                else
                    ( model, Cmd.none )

        Deal ->
            let
                ( gamecpct, moves ) =
                    Game.compact model.game
            in
            ( { model
                | game = Game.deal gamecpct
                , dealing = False
                , selected = List.map moves model.selected
                , answer = Nothing
              }
            , Cmd.none
            )

        DealMore ->
            let
                over =
                    Game.over model.game

                nsets =
                    Game.count model.game
            in
            if over then
                ( model, after 500 (GameOver model.log model.start) )
            else if nsets == 0 then
                ( { model | game = Game.dealMore model.game, answer = Nothing, log = EDealMoreZero :: model.log }, Cmd.none )
            else
                ( { model | answer = Just (Game.count model.game), log = EDealMoreNonzero :: model.log }, Cmd.none )

        _ ->
            Debug.crash "bad state"


score : List Event -> Time.Time -> Time.Time -> ( Int, String, Int )
score log start end =
    let
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
            List.length <| List.filter ((==) EDealMoreNonzero) <| log

        gooddeals =
            List.length <| List.filter ((==) EDealMoreZero) <| log

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


type alias Params =
    { style : Graphics.Style
    , key : Maybe String
    , name : Maybe String
    }


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
                        Graphics.standardSet

                    "modified" ->
                        Graphics.mySet

                    _ ->
                        Graphics.squareSet
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
