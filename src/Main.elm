port module Main exposing (..)

import Card exposing (..)
import Debug
import Dict
import Game exposing (Game)
import Html
import Html.Attributes as Html
import Html.Events as Html
import List.Extra
import Process
import Random
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import SvgSet
import Task
import Time


main =
    Html.program
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


type Model
    = Start (Maybe String)
    | Play GameModel


init : ( Model, Cmd Msg )
init =
    ( Start Nothing
    , Cmd.none
    )


style =
    SvgSet.squareSet


layout =
    SvgSet.squareLayout


view : Model -> Html.Html Msg
view model =
    case model of
        Start msg ->
            viewStart msg

        Play game ->
            viewGame game


viewGame : GameModel -> Html.Html Msg
viewGame model =
    let
        d ( pos, card ) =
            Svg.g
                [ SvgA.transform (trans pos)
                , SvgE.onClick (Choose pos)
                , SvgA.style "cursor: pointer;"
                ]
                [ SvgSet.draw layout style (List.member pos model.selected) card ]

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
                [ layout.button text ]

        trans ( c, r ) =
            let
                x =
                    (toFloat c + 0.5) * (10 + layout.w)

                y =
                    (toFloat r + 0.5) * (10 + layout.h)
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        viewBox =
            let
                width =
                    (layout.w + 10) * (toFloat cols + 1)

                height =
                    (layout.h + 10) * 3
            in
            "0 0 " ++ toString width ++ " " ++ toString height
    in
    Svg.svg
        [ SvgA.viewBox viewBox
        , Html.style [ ( "background", style.table ) ]
        ]
        (SvgSet.svgDefs style :: more :: gs)


viewStart : Maybe String -> Html.Html Msg
viewStart msg =
    let
        m =
            Maybe.withDefault "Start a game" msg
    in
    Html.div []
        [ Html.div [] [ Html.text m ]
        , Html.button [ Html.onClick <| Go False False ] [ Html.text "Full deck" ]
        , Html.button [ Html.onClick <| Go True False ] [ Html.text "Small deck" ]
        , Html.button [ Html.onClick <| Go False True ] [ Html.text "Superset (Full)" ]
        , Html.button [ Html.onClick <| Go True True ] [ Html.text "Superset (Small)" ]
        ]


type Msg
    = Go Bool Bool
    | NewGame Game
    | StartGame Time.Time
    | Choose Game.Pos
    | Deal
    | DealMore
    | GameOver (List Event) Time.Time Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Go short super ->
            ( model, Cmd.batch [ Random.generate NewGame (Game.init short super), Task.perform StartGame Time.now ] )

        NewGame game ->
            ( Play (initGame game), Cmd.none )

        GameOver log start now ->
            let
                ( secs, msg ) =
                    score log start now
            in
            ( Start (Just msg), logScore secs )

        _ ->
            case model of
                Play game ->
                    let
                        ( updgame, cmd ) =
                            updateGame msg game
                    in
                    ( Play updgame, cmd )

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


port logScore : Int -> Cmd msg


score : List Event -> Time.Time -> Time.Time -> ( Int, String )
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
    )
