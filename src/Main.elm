module Main exposing (..)

import Card exposing (..)
import Game exposing (..)
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
    List Card


init : ( Model, Cmd Msg )
init =
    ( [], Random.generate NewDeck shuffled )


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

        trans ( r, c ) =
            let
                x =
                    30 + 60 * c

                y =
                    50 + 90 * r
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        gs =
            zipWith d
                (List.map trans
                    [ ( 0, 0 )
                    , ( 0, 1 )
                    , ( 0, 2 )
                    , ( 0, 3 )
                    , ( 1, 0 )
                    , ( 1, 1 )
                    , ( 1, 2 )
                    , ( 1, 3 )
                    , ( 2, 0 )
                    , ( 2, 1 )
                    , ( 2, 2 )
                    , ( 2, 3 )
                    ]
                )
                cards
    in
    Svg.svg [ Svg.viewBox "0 0 300 300", Svg.width "500px" ]
        (SvgSet.svgDefs :: gs)


type Msg
    = NewDeck (List Card)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDeck cards ->
            ( cards, Cmd.none )
