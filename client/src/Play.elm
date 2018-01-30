module Play
    exposing
        ( Event(..)
        , LogEntry
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

import Card
import Dict
import Game exposing (Game, GameDef)
import Graphics
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Keyed as HtmlK
import List.Extra
import Random
import Svg
import Svg.Attributes as SvgA
import Svg.Events as SvgE
import Time


type alias Model =
    { def : GameDef
    , game : Game
    , log : List LogEntry
    , selected : List Game.Pos
    , hint : { match : List Game.Pos, count : Int }
    , dealing : Bool
    , answer : Maybe Int
    }


init : GameDef -> Game -> Model
init def game =
    { def = def
    , game = game
    , log = []
    , hint = { match = [], count = 0 }
    , selected = []
    , dealing = False
    , answer = Nothing
    }


type alias LogEntry =
    { time : Time.Time
    , event : Event
    }


type Event
    = EStart
    | EMatch
    | EMatchWrong
    | ENoMatch
    | ENoMatchWrong
    | EHint
    | EEnd


type Msg
    = StartGame
    | AutoDeal
    | AutoCompact
    | ChooseHint (List Game.Pos)
    | User UserMsg


type Result msg
    = After Time.Time msg
    | Command (Cmd msg)
    | GameOver (List LogEntry)


type UserMsg
    = Choose Game.Pos
    | UserDeal
    | UserHint


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
                    if model.dealing then
                        Nothing
                    else if model.answer /= Nothing && model.hint.count < gameView.matchSize - 1 then
                        Just UserHint
                    else if model.answer /= Nothing then
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
            { scores : List ( String, { present : Bool, score : Int } )
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

        offset ( x, y ) =
            [ ( "left", toString <| toFloat x * f ), ( "top", toString <| toFloat y * f ) ]

        d pos ( k, g ) =
            ( k
            , Svg.svg
                [ SvgA.width (px f)
                , SvgA.height (px f)
                , SvgA.viewBox "0 0 60 60"
                , HtmlA.style (offset pos)
                ]
                [ g ]
            )

        dcard pos card =
            ( "card-" ++ (toString <| Card.toInt card)
            , Svg.g
                [ SvgE.onClick (model.choose pos)
                , HtmlA.style [ ( "cursor", "pointer" ) ]
                , SvgA.transform "translate(30, 30)"
                ]
                [ Graphics.svgDefs model.style
                , Graphics.draw model.style (List.member pos model.selected) card
                ]
            )

        dempty =
            Svg.g
                [ SvgA.transform "translate(30, 30)"
                ]
                [ Graphics.button model.style ""
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
                                        |> Maybe.withDefault ( "empty-" ++ toString ( x, y ), dempty )
                                        |> d ( x, y )
                                )
                    )
                |> List.concat

        button pos =
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
            d pos <|
                ( "button"
                , Svg.g
                    (SvgA.transform "translate(30, 30)" :: handler)
                    [ Graphics.button model.style model.button.label
                    ]
                )

        infobox pos =
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
                    [ ( "infobox"
                      , Html.div
                            [ HtmlA.style (offset pos)
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
                                    (info.scores
                                        |> List.map
                                            (\( n, s ) ->
                                                Html.tr []
                                                    [ Html.td []
                                                        [ Html.text <|
                                                            if s.present then
                                                                "*"
                                                            else
                                                                ""
                                                        ]
                                                    , Html.td [] [ Html.text n ]
                                                    , Html.td [] [ Html.text <| toString s.score ]
                                                    ]
                                            )
                                    )
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
                      )
                    ]
    in
    HtmlK.node "div"
        [ HtmlA.id "game"
        , HtmlA.style [ ( "width", px width ), ( "height", px height ) ]
        ]
        (gs ++ (button ( cols, 0 ) :: infobox ( cols, 1 )))


update : Time.Time -> Msg -> Model -> ( Model, Maybe (Result Msg) )
update now msg model =
    case msg of
        StartGame ->
            let
                game =
                    Game.deal model.game
            in
            ( { model
                | log = [ { time = now, event = EStart } ]
                , game = game
              }
            , Just <| Command <| Random.generate ChooseHint (Game.randomMatch game)
            )

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
            let
                game =
                    Game.deal model.game
            in
            ( { model
                | game = game
                , dealing = False
              }
            , Just <| Command <| Random.generate ChooseHint (Game.randomMatch game)
            )

        ChooseHint match ->
            Debug.log ("chose hint: " ++ toString match) <|
                ( { model | hint = { match = match, count = 0 } }, Nothing )

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
                            { time = now, event = EMatch } :: model.log
                    in
                    ( { model
                        | game = newgame
                        , selected = []
                        , dealing = True
                        , answer = Nothing
                        , log = { time = now, event = EMatch } :: model.log
                      }
                    , Just (After 250 AutoCompact)
                    )
                else
                    ( { model | log = { time = now, event = EMatchWrong } :: model.log }
                    , Nothing
                    )

        User UserDeal ->
            let
                over =
                    Game.over model.game

                nmatches =
                    Game.count model.game
            in
            if over then
                ( model, Just <| GameOver <| { time = now, event = EEnd } :: model.log )
            else if nmatches == 0 then
                let
                    game =
                        Game.dealMore model.game
                in
                ( { model | game = game, answer = Nothing, log = { time = now, event = ENoMatch } :: model.log }
                , Just <| Command <| Random.generate ChooseHint (Game.randomMatch game)
                )
            else
                ( { model | answer = Just (Game.count model.game), log = { time = now, event = ENoMatchWrong } :: model.log }, Nothing )

        User UserHint ->
            let
                oldHint =
                    model.hint

                count =
                    oldHint.count + 1
            in
            ( { model
                | hint = { oldHint | count = count }
                , selected = List.take count oldHint.match
                , log = { time = now, event = EHint } :: model.log
              }
            , Nothing
            )
