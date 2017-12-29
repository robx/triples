module Main exposing (..)

import Card exposing (..)
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
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


view : Model -> Html.Html Msg
view card =
    let
        card1 =
            { color = Red, count = Three, shape = Oval, fill = Empty }

        card2 =
            { color = Green, count = One, shape = Diamond, fill = Full }

        card3 =
            { color = Purple, count = Two, shape = Squiggle, fill = Shaded }
    in
    Svg.svg [ Svg.viewBox "-100 -50 200 100", Svg.width "300px" ]
        [ SvgSet.clipPaths
        , Svg.g [ Svg.transform "translate(-60,0)" ] [ SvgSet.draw card1 ]
        , Svg.g [ Svg.transform "translate(  0,0)" ] [ SvgSet.draw card2 ]
        , Svg.g [ Svg.transform "translate( 60,0)" ] [ SvgSet.draw card3 ]
        ]


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update () m =
    ( m, Cmd.none )
