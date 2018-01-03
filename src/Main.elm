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
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.none, selected = [], dealing = False }, Random.generate NewGame Game.init )


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
                    50 + 90 * r
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        gs =
            Dict.toList model.game.table |> List.map d
    in
    Html.div []
        [ Html.button
            [ Html.onClick DealMore
            , Html.disabled <| model.dealing || Game.deckEmpty model.game
            ]
            [ Html.text "Deal more" ]
        , Svg.svg [ Svg.viewBox "0 0 300 300", Svg.width "500px" ]
            (SvgSet.svgDefs :: gs)
        ]


type Msg
    = NewGame Game
    | Choose Game.Pos
    | Deal
    | DealMore


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        after time msg =
            Process.sleep time |> Task.perform (always msg)
    in
    case msg of
        NewGame game ->
            ( { game = game, selected = [], dealing = False }, Cmd.none )

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
                    ( { model | game = newgame, selected = [], dealing = True }, after 1000 Deal )
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
              }
            , Cmd.none
            )

        DealMore ->
            ( { model | game = Game.dealMore model.game }, Cmd.none )
