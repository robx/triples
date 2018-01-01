module Main exposing (..)

import Card exposing (..)
import Dict
import Game exposing (Game)
import Html
import List.Extra
import Random
import Svg
import Svg.Attributes as Svg
import Svg.Events as Svg
import SvgSet


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
    }


init : ( Model, Cmd Msg )
init =
    ( { game = Game.none, selected = [] }, Random.generate NewGame Game.init )


view : Model -> Html.Html Msg
view model =
    let
        d ( pos, card ) =
            Svg.g
                [ Svg.transform (trans pos)
                , Svg.onClick (Choose pos)
                ]
                [ SvgSet.draw (List.member pos model.selected) card ]

        trans ( r, c ) =
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
    Svg.svg [ Svg.viewBox "0 0 300 300", Svg.width "500px" ]
        (SvgSet.svgDefs :: gs)


type Msg
    = NewGame Game
    | Choose Game.Pos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame game ->
            ( { game = game, selected = [] }, Cmd.none )

        Choose p ->
            if Game.empty model.game p then
                ( model, Cmd.none )
            else if List.member p model.selected then
                ( { model | selected = List.Extra.remove p model.selected }, Cmd.none )
            else if List.length model.selected < 2 then
                ( { model | selected = p :: model.selected } , Cmd.none )
            else
                let
                    ( set, newgame ) =
                        Game.take model.game (p :: model.selected)
                in
                if set then
                    ( { model | game = newgame, selected = [] }, Cmd.none )
                else
                    ( model, Cmd.none )
