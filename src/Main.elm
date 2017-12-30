module Main exposing (..)

import Card exposing (..)
import Game exposing (..)
import Html
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
    List Card


init : ( Model, Cmd Msg )
init =
    let
        ( cards, _ ) =
            deal deck
    in
    ( cards, Cmd.none )


view : Model -> Html.Html Msg
view cards =
    let
        zipWith f xs ys =
            case ( xs, ys ) of
                ( x :: xss, y :: yss ) ->
                    f x y :: zipWith f xss yss

                _ ->
                    []

        d t c =
            Svg.g [ Svg.transform t ] [ SvgSet.draw c ]

        gs =
            zipWith d
                [ "translate(30,50)"
                , "translate(90,50)"
                , "translate(150,50)"
                , "translate(210,50)"
                , "translate(30,140)"
                , "translate(90,140)"
                , "translate(150,140)"
                , "translate(210,140)"
                , "translate(30,230)"
                , "translate(90,230)"
                , "translate(150,230)"
                , "translate(210,230)"
                ]
                cards
    in
    Svg.svg [ Svg.viewBox "0 0 300 300", Svg.width "500px" ]
        (SvgSet.svgDefs :: gs)


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update () m =
    ( m, Cmd.none )
