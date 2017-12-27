module Main exposing (..)

import Card
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
    Card.Card


init : ( Model, Cmd Msg )
init =
    ( Card.sampleCard, Cmd.none )


view : Model -> Html.Html Msg
view card =
    Svg.svg [ Svg.viewBox "0 0 100 100", Svg.width "300px" ]
        [ Svg.g
            [ Svg.transform "translate(10,10)" ]
            [ SvgSet.draw card ]
        ]


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update () m =
    ( m, Cmd.none )
