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
    | AutoDeal
    | User UserMsg


type Result msg
    = After Time.Time msg
    | GameOver (List ( Time.Time, Event ))


type UserMsg
    = Choose Game.Pos
    | UserDeal


view : Style.Style -> Model -> Html.Html Msg
view style model =
    let
        gameView =
            Game.toView model.game
    in
    Html.map User <|
        viewGame
            { style = style
            , game = gameView
            , selected = model.selected
            , button =
                { message =
                    if model.dealing || model.answer /= Nothing then
                        Nothing
                    else
                        Just UserDeal
                , label =
                    case model.answer of
                        Just n ->
                            toString n

                        Nothing ->
                            if gameView.deckSize == 0 then
                                "."
                            else
                                "+"
                }
            , choose = Choose
            , info = Nothing
            }


type alias ViewGameModel msg =
    { style : Style.Style
    , game : Game.GameView
    , selected : List Game.Pos
    , button : { message : Maybe msg, label : String }
    , choose : Game.Pos -> msg
    , info :
        Maybe
            { scores : List ( String, Int )
            , events : List String
            }
    }


viewGame : ViewGameModel msg -> Html.Html msg
viewGame model =
    let
        cols =
            Game.viewColumns model.game

        position pos =
            let
                ( x, y ) =
                    pos
            in
            [ ( "grid-column", toString (x + 1) ++ "/" ++ toString (x + 2) )
            , ( "grid-row", toString (y + 1) ++ "/" ++ toString (y + 2) )
            ]

        d ( pos, card ) =
            Svg.svg [ HtmlA.style <| position pos, SvgA.viewBox "-30 -30 60 60" ]
                [ Svg.g
                    [ SvgE.onClick (model.choose pos)
                    , HtmlA.style [ ( "cursor", "pointer" ) ]
                    ]
                    [ Graphics.svgDefs model.style
                    , Graphics.draw model.style (List.member pos model.selected) card
                    ]
                ]

        gs =
            Dict.toList model.game.table |> List.map d

        button =
            let
                handler =
                    case model.button.message of
                        Nothing ->
                            []

                        Just msg ->
                            [ SvgE.onClick msg
                            , SvgA.style "cursor: pointer;"
                            ]
            in
            Svg.svg [ HtmlA.style <| position ( cols, 0 ), SvgA.viewBox "-30 -30 60 60" ]
                [ Svg.g
                    handler
                    [ Graphics.svgDefs model.style
                    , Graphics.button model.style model.button.label
                    ]
                ]

        {-
           infobox =
               case model.info of
                   Nothing ->
                       []

                   Just info ->
                       let
                           w =
                               model.style.layout.w

                           h =
                               2 * model.style.layout.h + 10
                       in
                       [ Svg.g [ SvgA.transform (trans ( toFloat cols, 1.5 )) ]
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
                                       [ HtmlA.id "infobox" ]
                                       [ Html.table []
                                           (List.map (\( n, s ) -> Html.tr [] [ Html.td [] [ Html.text <| n ], Html.td [] [ Html.text <| toString s ] ]) info.scores)
                                       , Html.ul [] <|
                                           List.map (\e -> Html.li [] [ Html.text e ]) info.events
                                       ]
                                   ]
                               ]
                           ]
                       ]
        -}
    in
    Html.div
        [ HtmlA.class "main" ]
        [ Html.div
            [ HtmlA.id "game"
            , HtmlA.style [ ( "display", "grid" ) ]
            ]
            (button :: gs)
        ]


update : Time.Time -> Msg -> Model -> ( Model, Maybe (Result Msg) )
update now msg model =
    case msg of
        StartGame ->
            ( { model | log = [ ( now, EStart ) ] }, Nothing )

        AutoDeal ->
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
                    , Just (After 500 AutoDeal)
                    )
                else
                    ( model, Nothing )

        User UserDeal ->
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
