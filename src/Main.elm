module Main exposing (..)

import Card exposing (..)
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


type alias Model =
    { game : Game
    , start : Time.Time
    , log : List Event
    , selected : List Game.Pos
    , dealing : Bool
    , answer : Maybe Int
    , message : Maybe String
    }


type Event
    = ESet
    | EDealMore
    | EAsk


init : ( Model, Cmd Msg )
init =
    ( { game = Game.none, selected = [], dealing = False, answer = Nothing, start = 0, log = [], message = Just "Start!" }
    , Cmd.none
    )


style =
    SvgSet.mySet


view : Model -> Html.Html Msg
view model =
    let
        d ( pos, card ) =
            Svg.g
                [ Svg.transform (trans pos)
                , Svg.onClick (Choose pos)
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
                handler =
                    if model.dealing || Game.deckEmpty model.game then
                        []
                    else
                        [ Svg.onClick DealMore ]
            in
            Svg.g
                (Svg.transform (trans ( cols, 0 )) :: handler)
                [ SvgSet.letterCard "+" ]

        ask =
            case model.answer of
                Nothing ->
                    let
                        handler =
                            if model.dealing then
                                []
                            else
                                [ Svg.onClick Ask ]
                    in
                    Svg.g
                        [ Svg.transform (trans ( cols, 1 ))
                        , Svg.onClick Ask
                        ]
                        [ SvgSet.letterCard "?" ]

                Just n ->
                    Svg.g
                        [ Svg.transform (trans ( cols, 1 ))
                        ]
                        [ SvgSet.letterCard <| toString n ]

        viewBox =
            let
                width =
                    (50 + 10) * (cols + 1)

                height =
                    (80 + 10) * 3
            in
            "0 0 " ++ toString width ++ " " ++ toString height
    in
    case model.message of
        Nothing ->
            Svg.svg
                [ Svg.viewBox viewBox
                , Html.style [ ( "background", style.table ) ]
                ]
                (SvgSet.svgDefs style :: more :: ask :: gs)

        Just m ->
            Html.div [ Html.class "message", Html.onClick Go ] [ Html.text m ]


type Msg
    = Go
    | NewGame Game
    | StartGame Time.Time
    | Choose Game.Pos
    | Deal
    | DealMore
    | Ask
    | GameOver Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
        Go ->
            ( model, Cmd.batch [ Random.generate NewGame Game.init, Task.perform StartGame Time.now ] )

        NewGame game ->
            ( { model | game = game }, Cmd.none )

        StartGame start ->
            ( { model | start = start, message = Nothing }, Cmd.none )

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
                    ( { model | game = newgame, selected = [], dealing = True, answer = Nothing, log = ESet :: model.log }
                    , after 500 <|
                        if over then
                            GameOver
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
            ( { model | game = Game.dealMore model.game, answer = Nothing, log = EDealMore :: model.log }, Cmd.none )

        Ask ->
            ( { model | answer = Just (Game.countSets model.game), log = EAsk :: model.log }, Cmd.none )

        GameOver now ->
            ( { model | message = Just (score model.log model.start now) }, Cmd.none )


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
            List.length <| List.filter ((==) EDealMore) <| log

        dealsecs =
            deals * 60

        asks =
            List.length <| List.filter ((==) EAsk) <| log

        asksecs =
            asks * 20
    in
    format (secs + dealsecs + asksecs) ++ " = " ++ format secs ++ " + " ++ format asksecs ++ " + " ++ format dealsecs
