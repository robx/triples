module Play
    exposing
        ( Event(..)
        , Model
        , Msg(..)
        , Result(..)
        , UserMsg(..)
        , init
        , update
        , view
        , viewGame
        )

import Dict
import Game exposing (Game)
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import List.Extra
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Time


type alias Model =
    { game : Game
    , log : List ( Time.Time, Event )
    , selected : List Game.Pos
    , dealing : Bool
    , answer : Maybe Int
    }


init : Game -> Model
init game =
    { game = game
    , log = []
    , selected = []
    , dealing = False
    , answer = Nothing
    }


type Event
    = EStart
    | ESet
    | EDealMoreZero
    | EDealMoreNonzero
    | EEnd


type Msg
    = StartGame
    | Deal
    | User UserMsg


type Result msg
    = After Time.Time msg
    | GameOver (List ( Time.Time, Event ))


type UserMsg
    = Choose Game.Pos
    | DealMore


view : Style.Style -> Model -> Html.Html Msg
view style model =
    let
        disableMore =
            model.dealing
    in
    Html.map User <|
        viewGame style (Game.toView model.game) model.selected disableMore model.answer


viewGame : Style.Style -> Game.GameView -> List Game.Pos -> Bool -> Maybe Int -> Html.Html UserMsg
viewGame style gameView selected disableMore answer =
    let
        d ( pos, card ) =
            Svg.g
                [ SvgA.transform (trans pos)
                , SvgE.onClick (Choose pos)
                , SvgA.style "cursor: pointer;"
                ]
                [ Graphics.draw style (List.member pos selected) card ]

        gs =
            Dict.toList gameView.table |> List.map d

        more =
            let
                text =
                    case answer of
                        Just n ->
                            toString n

                        Nothing ->
                            if gameView.deckSize == 0 then
                                "."
                            else
                                "+"

                disabled =
                    disableMore || answer /= Nothing

                handler =
                    if disabled then
                        []
                    else
                        [ SvgE.onClick DealMore
                        , SvgA.style "cursor: pointer;"
                        ]
            in
            Svg.g
                (SvgA.transform (trans ( gameView.cols, 0 )) :: handler)
                [ Graphics.button style text ]

        trans ( c, r ) =
            let
                x =
                    (toFloat c + 0.5) * (10 + style.layout.w)

                y =
                    (toFloat r + 0.5) * (10 + style.layout.h)
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        viewBox =
            let
                width =
                    (style.layout.w + 10) * (toFloat gameView.cols + 1)

                height =
                    (style.layout.h + 10) * toFloat gameView.rows
            in
            "0 0 " ++ toString width ++ " " ++ toString height
    in
    Svg.svg
        [ SvgA.viewBox viewBox
        , HtmlA.id "main"
        , HtmlA.style [ ( "background", style.colors.table ) ]
        ]
        (Graphics.svgDefs style :: more :: gs)


update : Time.Time -> Msg -> Model -> ( Model, Maybe (Result Msg) )
update now msg model =
    case msg of
        StartGame ->
            ( { model | log = [ ( now, EStart ) ] }, Nothing )

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
            , Nothing
            )

        User (Choose p) ->
            if Game.posEmpty model.game p then
                ( model, Nothing )
            else if List.member p model.selected then
                ( { model | selected = List.Extra.remove p model.selected }, Nothing )
            else if List.length model.selected < (Game.gameMatchSize model.game - 1) then
                ( { model | selected = p :: model.selected }, Nothing )
            else
                let
                    ( isset, newgame ) =
                        Game.take model.game (p :: model.selected)
                in
                if isset then
                    let
                        log =
                            ( now, ESet ) :: model.log
                    in
                    ( { model | game = newgame, selected = [], dealing = True, answer = Nothing, log = log }
                    , Just (After 500 Deal)
                    )
                else
                    ( model, Nothing )

        User DealMore ->
            let
                over =
                    Game.over model.game

                nsets =
                    Game.count model.game
            in
            if over then
                ( model, Just <| GameOver <| ( now, EEnd ) :: model.log )
            else if nsets == 0 then
                ( { model | game = Game.dealMore model.game, answer = Nothing, log = ( now, EDealMoreZero ) :: model.log }, Nothing )
            else
                ( { model | answer = Just (Game.count model.game), log = ( now, EDealMoreNonzero ) :: model.log }, Nothing )
