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
    Html.map User <|
        viewGame
            { style = style
            , game = Game.toView model.game
            , selected = model.selected
            , disableMore = model.dealing
            , answer = model.answer
            , events = []
            }


type alias ViewGameModel =
    { style : Style.Style
    , game : Game.GameView
    , selected : List Game.Pos
    , disableMore : Bool
    , answer : Maybe Int
    , events : List String
    }


viewGame : ViewGameModel -> Html.Html UserMsg
viewGame model =
    let
        d ( pos, card ) =
            let
                ( x, y ) =
                    pos
            in
            Svg.g
                [ SvgA.transform (trans ( toFloat x, toFloat y ))
                , SvgE.onClick (Choose pos)
                , SvgA.style "cursor: pointer;"
                ]
                [ Graphics.draw model.style (List.member pos model.selected) card ]

        gs =
            Dict.toList model.game.table |> List.map d

        more =
            let
                text =
                    case model.answer of
                        Just n ->
                            toString n

                        Nothing ->
                            if model.game.deckSize == 0 then
                                "."
                            else
                                "+"

                disabled =
                    model.disableMore || model.answer /= Nothing

                handler =
                    if disabled then
                        []
                    else
                        [ SvgE.onClick DealMore
                        , SvgA.style "cursor: pointer;"
                        ]
            in
            Svg.g
                (SvgA.transform (trans ( toFloat model.game.cols, 0 )) :: handler)
                [ Graphics.button model.style text ]

        trans ( c, r ) =
            let
                x =
                    (c + 0.5) * (10 + model.style.layout.w)

                y =
                    (r + 0.5) * (10 + model.style.layout.h)
            in
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"

        viewBox =
            let
                width =
                    (model.style.layout.w + 10) * (toFloat model.game.cols + 1)

                height =
                    (model.style.layout.h + 10) * toFloat model.game.rows
            in
            "0 0 " ++ toString width ++ " " ++ toString height

        events =
            case model.events of
                [] ->
                    []

                es ->
                    let
                        w =
                            model.style.layout.w

                        h =
                            2 * model.style.layout.h + 10
                    in
                    [ Svg.g [ SvgA.transform (trans ( toFloat model.game.cols, 1.5 )) ]
                        [ Svg.rect
                            [ SvgA.width (toString w)
                            , SvgA.height (toString h)
                            , SvgA.x (toString (-w / 2))
                            , SvgA.y (toString (-h / 2))
                            , SvgA.rx "6"
                            , SvgA.ry "6"
                            , SvgA.fill "lightgray"
                            ]
                            []
                        , Svg.g [ SvgA.transform "scale(0.333333)" ]
                            [ Svg.foreignObject
                                [ SvgA.width (toString (3 * (w - 5)))
                                , SvgA.height (toString (3 * (h - 5)))
                                , SvgA.x (toString (-3 / 2 * (w - 5)))
                                , SvgA.y (toString (-3 / 2 * (h - 5)))
                                ]
                                [ Html.div
                                    [ HtmlA.id "messages" ]
                                    [ Html.ul [] <|
                                        List.map (\e -> Html.li [] [ Html.text e ]) es
                                    ]
                                ]
                            ]
                        ]
                    ]
    in
    Svg.svg
        [ SvgA.viewBox viewBox
        , SvgA.width "800px"
        , SvgA.height "600px"
        , HtmlA.id "main"
        , HtmlA.style [ ( "background", model.style.colors.table ) ]
        ]
        (Graphics.svgDefs model.style :: more :: events ++ gs)


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
