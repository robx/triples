module Menu
    exposing
        ( Model
        , view
        )

import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type alias Model =
    Maybe String


view : (Bool -> Bool -> msg) -> Maybe String -> Style.Style -> Model -> Html.Html msg
view go name style score =
    let
        addScore h =
            case score of
                Just m ->
                    Html.div [ HtmlA.class "msg", HtmlA.style [ ( "background", snd style.colors.symbols ) ] ] [ Html.text m ] :: h

                Nothing ->
                    h

        prompt =
            "Choose a game"
                ++ (case name of
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
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , Html.div [ HtmlA.class "buttons" ]
                [ Html.button [ HtmlE.onClick <| go False False ] [ Html.text "Classic (scored!)" ]
                , Html.button [ HtmlE.onClick <| go True False ] [ Html.text "Classic (short)" ]
                , Html.button [ HtmlE.onClick <| go False True ] [ Html.text "Super" ]
                , Html.button [ HtmlE.onClick <| go True True ] [ Html.text "Super (short)" ]
                ]
            ]
