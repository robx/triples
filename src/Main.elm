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
import Svg.Attributes as Svg
import Svg.Attributes
import Svg.Events as Svg
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
    SvgSet.mySet


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
                [ Svg.transform (trans pos)
                , Svg.onClick (Choose pos)
                , Svg.Attributes.style "cursor: pointer;"
                ]
                [ SvgSet.draw style (List.member pos model.selected) card ]

        trans ( c, r ) =
            let
                x =
                    30 + 60 * c

                y =
                    45 + 90 * r
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

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

                handler =
                    if model.dealing || Game.deckEmpty model.game || model.answer /= Nothing then
                        []
                    else
                        [ Svg.onClick DealMore
                        , Svg.Attributes.style "cursor: pointer;"
                        ]
            in
            Svg.g
                (Svg.transform (trans ( cols, 0 )) :: handler)
                [ SvgSet.letterCard text ]

        viewBox =
            let
                width =
                    (50 + 10) * (cols + 1)

                height =
                    (80 + 10) * 3
            in
            "0 0 " ++ toString width ++ " " ++ toString height
    in
    Svg.svg
        [ Svg.viewBox viewBox
        , Html.style [ ( "background", style.table ) ]
        ]
        (SvgSet.svgDefs style :: more :: gs)


viewStart : Maybe String -> Html.Html Msg
viewStart msg =
    let
        m =
            Maybe.withDefault "Start!" msg
    in
    Html.div []
        [ Html.text m
        , Html.button [ Html.onClick Go ] [ Html.text "Go!" ]
        , Html.button [ Html.onClick GoShort ] [ Html.text "Short!" ]
        ]


type Msg
    = Go
    | GoShort
    | NewGame Game
    | StartGame Time.Time
    | Choose Game.Pos
    | Deal
    | DealMore
    | GameOver (List Event) Time.Time Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Go ->
            ( model, Cmd.batch [ Random.generate NewGame Game.init, Task.perform StartGame Time.now ] )

        GoShort ->
            ( model, Cmd.batch [ Random.generate NewGame Game.initShort, Task.perform StartGame Time.now ] )

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
            else if List.length model.selected < 2 then
                ( { model | selected = p :: model.selected }, Cmd.none )
            else
                let
                    ( isset, newgame ) =
                        Game.take model.game (p :: model.selected)

                    over =
                        Game.over newgame
                in
                if isset then
                    let
                        log =
                            ESet :: model.log
                    in
                    ( { model | game = newgame, selected = [], dealing = True, answer = Nothing, log = log }
                    , after 500 <|
                        if over then
                            GameOver log model.start
                        else
                            always Deal
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
                nsets =
                    Game.countSets model.game
            in
            if nsets == 0 then
                ( { model | game = Game.dealMore model.game, answer = Nothing, log = EDealMoreZero :: model.log }, Cmd.none )
            else
                ( { model | answer = Just (Game.countSets model.game), log = EDealMoreNonzero :: model.log }, Cmd.none )

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

        deals =
            List.length <| List.filter ((==) EDealMoreNonzero) <| log

        dealsecs =
            deals * 60

        totalsecs =
            secs + dealsecs
    in
    ( totalsecs
    , String.join " " [ format totalsecs, "=", format secs, "=", format dealsecs ]
    )
