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


view : (Bool -> Bool -> msg) -> Model -> Html.Html msg
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
    in
    Html.div [ HtmlA.id "main" ] <|
        addScore
            [ Html.div
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd model.style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , case model.telegramGame of
                Nothing ->
                    Html.div [ HtmlA.class "buttons" ] <|
                        [ Html.button [ HtmlE.onClick <| go False False ] [ Html.text "Classic (scored!)" ]
                        , Html.button [ HtmlE.onClick <| go True False ] [ Html.text "Classic (short)" ]
                        , Html.button [ HtmlE.onClick <| go False True ] [ Html.text "Super" ]
                        , Html.button [ HtmlE.onClick <| go True True ] [ Html.text "Super (short)" ]
                        ]

                Just Game.ClassicSet ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| go False False ] [ Html.text "Play Triples!" ] ]

                Just Game.SuperSet ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| go False True ] [ Html.text "Play Quadruples!" ] ]
            ]
