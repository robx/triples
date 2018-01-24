module Play
    exposing
        ( Event(..)
        , Model
        , Msg(..)
        , Result(..)
        , Size
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
    | AutoCompact
    | User UserMsg


type Result msg
    = After Time.Time msg
    | GameOver (List ( Time.Time, Event ))


type UserMsg
    = Choose Game.Pos
    | UserDeal


type alias Size =
    { w : Int
    , h : Int
    }


view : Style.Style -> Size -> Model -> Html.Html Msg
view style maxSize model =
    let
        gameView =
            Game.toView model.game
    in
    Html.map User <|
        viewGame
            { style = style
            , maxSize = maxSize
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
    , maxSize : Size
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

        rows =
            model.game.rows

        h =
            toFloat rows

        w =
            toFloat (cols + 1)

        width =
            min model.maxSize.w <| round (toFloat model.maxSize.h / h * w)

        height =
            min model.maxSize.h <| round (toFloat model.maxSize.w / w * h)

        f =
            toFloat width / w

        px n =
            toString n ++ "px"

        d g =
            Svg.svg [ SvgA.width (px f), SvgA.height (px f), SvgA.viewBox "0 0 60 60" ]
                [ g ]

        dcard pos card =
            Svg.g
                [ SvgE.onClick (model.choose pos)
                , HtmlA.style [ ( "cursor", "pointer" ) ]
                , SvgA.transform "translate(30, 30)"
                ]
                [ Graphics.svgDefs model.style
                , Graphics.draw model.style (List.member pos model.selected) card
                ]

        dempty =
            Svg.g
                [ SvgA.transform "translate(30, 30)"
                ]
                [ Graphics.svgDefs model.style
                , Graphics.button model.style ""
                ]

        gs =
            List.range 0 (cols - 1)
                |> List.map
                    (\x ->
                        List.range 0 (rows - 1)
                            |> List.map
                                (\y ->
                                    Dict.get ( x, y ) model.game.table
                                        |> Maybe.map (dcard ( x, y ))
                                        |> Maybe.withDefault dempty
                                        |> d
                                )
                    )
                |> List.concat

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
            d <|
                Svg.g
                    (SvgA.transform "translate(30, 30)" :: handler)
                    [ Graphics.svgDefs model.style
                    , Graphics.button model.style model.button.label
                    ]

        infobox =
            case model.info of
                Nothing ->
                    []

                Just info ->
                    let
                        iw =
                            px f

                        ih =
                            px (2 * f)
                    in
                    [ Html.div
                        [ HtmlA.style
                            [ ( "position", "relative" ) ]
                        ]
                        [ Html.div
                            [ HtmlA.id "infobox"
                            , HtmlA.style
                                [ ( "position", "absolute" )
                                , ( "top", px (0.15 * f) )
                                , ( "left", px (0.15 * f) )
                                , ( "width", px (0.7 * f) )
                                , ( "height", px (1.7 * f) )
                                , ( "z-index", "1" )
                                , ( "font-size", px (0.1 * f) )
                                ]
                            ]
                            [ Html.table
                                [ HtmlA.style [ ( "font-size", px (0.1 * f) ) ] ]
                                (info.scores |> List.map (\( n, s ) ->
                                    Html.tr []
                                        [ Html.td [] [ Html.text <| n ]
                                        , Html.td [] [ Html.text <| toString s ] ]))
                            , Html.ul [] <|
                                List.map (\e -> Html.li [] [ Html.text e ]) info.events
                            ]
                        , Svg.svg
                            [ SvgA.width iw
                            , SvgA.height ih
                            , SvgA.viewBox "0 0 60 120"
                            , HtmlA.style [ ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ]
                            ]
                            [ Svg.rect
                                [ SvgA.width "50"
                                , SvgA.height "110"
                                , SvgA.x "5"
                                , SvgA.y "5"
                                , SvgA.rx "6"
                                , SvgA.ry "6"
                                , SvgA.fill "lightgray"
                                ]
                                []
                            ]
                        ]
                    ]
    in
    Html.div
        [ HtmlA.id "game"
        , SvgA.style <| "display: grid; grid-template-columns: repeat(" ++ toString (cols + 1) ++ ",1fr); grid-template-rows: repeat(3,1fr); width: " ++ px width ++ "; height: " ++ px height ++ ";"

        {- , HtmlA.style
           [ ( "display", "grid" )
           , ( "grid-template-columns", "repeat(" ++ toString (cols + 1) ++ ",fr)" )
           , ( "grid-template-rows", "repeat(3,fr)" )
           , ( "grrrrr", "grrrrr")
           , ( "background-color", "green" )
           , ( "width", px width )
           , ( "height", px height )
           ]
        -}
        ]
        (gs ++ (button :: infobox))


update : Time.Time -> Msg -> Model -> ( Model, Maybe (Result Msg) )
update now msg model =
    case msg of
        StartGame ->
            ( { model | log = [ ( now, EStart ) ] }, Nothing )

        AutoCompact ->
            let
                ( gamecpct, moves ) =
                    Game.compact model.game
            in
            ( { model
                | game = gamecpct
                , selected = List.map moves model.selected
              }
            , Just <| After 250 AutoDeal
            )

        AutoDeal ->
            ( { model
                | game = Game.deal model.game
                , dealing = False
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
                    , Just (After 250 AutoCompact)
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
