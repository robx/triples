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


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { game : Game
    , selected : List Game.Pos
    , dealing : Bool
    , answer : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.none, selected = [], dealing = False, answer = Nothing }, Random.generate NewGame Game.init )


view : Model -> Html.Html Msg
view model =
    let
        d ( pos, card ) =
            Svg.g
                [ Svg.transform (trans pos)
                , Svg.onClick (Choose pos)
                ]
                [ SvgSet.draw (List.member pos model.selected) card ]

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
    Svg.svg [ Svg.viewBox viewBox ]
        (SvgSet.svgDefs :: more :: ask :: gs)


type Msg
    = NewGame Game
    | Choose Game.Pos
    | Deal
    | DealMore
    | Ask


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        after time msg =
            Process.sleep time |> Task.perform (always msg)
    in
    case msg of
        NewGame game ->
            ( { game = game, selected = [], dealing = False, answer = Nothing }, Cmd.none )

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
                in
                if isset then
                    ( { model | game = newgame, selected = [], dealing = True, answer = Nothing }, after 1000 Deal )
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
            ( { model | game = Game.dealMore model.game, answer = Nothing }, Cmd.none )

        Ask ->
            ( { model | answer = Just (Game.countSets model.game) }, Cmd.none )
