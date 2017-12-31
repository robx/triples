module Main exposing (..)

import Card exposing (..)
import Dict
import Game exposing (Game)
import Html
import Random
import Svg
import Svg.Attributes as Svg
import SvgSet


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    Game


init : ( Model, Cmd Msg )
init =
    ( Game.none, Random.generate NewGame Game.init )


view : Model -> Html.Html Msg
view game =
    let
        d ( pos, card ) =
            Svg.g [ Svg.transform (trans pos) ] [ SvgSet.draw card ]

        trans ( r, c ) =
            let
                x =
                    30 + 60 * c

                y =
                    50 + 90 * r
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        gs =
            Dict.toList game.table |> List.map d
    in
    Svg.svg [ Svg.viewBox "0 0 300 300", Svg.width "500px" ]
        (SvgSet.svgDefs :: gs)


type Msg
    = NewGame Game


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame game ->
            ( game, Cmd.none )
