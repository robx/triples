module Menu
    exposing
        ( Model
        , view
        )

import Game
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type alias Model =
    { score : Maybe String
    , telegramGame : Maybe Game.GameType
    , name : Maybe String
    , style : Style.Style
    }


view : (Game.GameDef -> msg) -> Model -> Html.Html msg
view go model =
    let
        addScore h =
            case model.score of
                Just m ->
                    Html.div [ HtmlA.class "msg", HtmlA.style [ ( "background", snd model.style.colors.symbols ) ] ] [ Html.text m ] :: h

                Nothing ->
                    h

        prompt =
            "Choose a game"
                ++ (case model.name of
                        Just n ->
                            ", " ++ n ++ "!"

                        Nothing ->
                            "!"
                   )

        fst ( x, y, z ) =
            x

        snd ( x, y, z ) =
            y

        trd ( x, y, z ) =
            z

        def =
            { type_ = Game.ClassicSet, short = False, multi = False }
    in
    Html.div [ HtmlA.id "main" ] <|
        addScore
            [ Html.div
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd model.style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , case model.telegramGame of
                Nothing ->
                    Html.div [ HtmlA.class "buttons" ] <|
                        [ Html.button [ HtmlE.onClick <| go def ] [ Html.text "Classic" ]
                        , Html.button [ HtmlE.onClick <| go { def | short = True } ] [ Html.text "Classic (short)" ]
                        , Html.button [ HtmlE.onClick <| go { def | type_ = Game.SuperSet } ] [ Html.text "Super" ]
                        , Html.button [ HtmlE.onClick <| go { def | type_ = Game.SuperSet, short = True } ] [ Html.text "Super (short)" ]
                        ]

                Just Game.ClassicSet ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| go def ] [ Html.text "Play Triples!" ] ]

                Just Game.SuperSet ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| go { def | type_ = Game.SuperSet } ] [ Html.text "Play Quadruples!" ] ]
            ]
