module Menu
    exposing
        ( Model
        , Msg(..)
        , Score
        , update
        , view
        )

import Game
import Graphics.Style as Style
import Html
import Html.Attributes as HtmlA
import Html.Events as HtmlE


type alias Score =
    { summary : String
    , details : List String
    }


type alias Model =
    { score : Maybe Score
    , scoreDetails : Bool
    , telegramGame : Maybe Game.GameDef
    , name : Maybe String
    , style : Style.Style
    }


type Msg
    = ToggleDetails


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDetails ->
            { model | scoreDetails = not model.scoreDetails }


view : (Msg -> msg) -> (Game.GameDef -> msg) -> Model -> Html.Html msg
view wrap go model =
    let
        addScore h =
            case model.score of
                Nothing ->
                    h

                Just sc ->
                    let
                        details =
                            if model.scoreDetails then
                                sc.details |> List.map (\m -> Html.p [] [ Html.text m ])

                            else
                                []

                        head =
                            Html.h4 [ HtmlE.onClick (wrap ToggleDetails) ] [ Html.text sc.summary ]
                    in
                    Html.div [ HtmlA.class "msg", HtmlA.style [ ( "background", snd model.style.colors.symbols ) ] ]
                        (head :: details)
                        :: h

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
            { type_ = Game.Triples, short = False, multi = False }
    in
    Html.div [ HtmlA.id "menu" ] <|
        addScore
            [ Html.div
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd model.style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , case model.telegramGame of
                Nothing ->
                    Html.div [ HtmlA.class "buttons" ] <|
                        [ Html.button [ HtmlE.onClick <| go def ] [ Html.text "Classic" ]
                        , Html.button [ HtmlE.onClick <| go { def | short = True } ] [ Html.text "Classic (short)" ]
                        , Html.button [ HtmlE.onClick <| go { def | multi = True } ] [ Html.text "Classic (multiplayer)" ]
                        , Html.button [ HtmlE.onClick <| go { def | type_ = Game.Quadruples } ] [ Html.text "Super" ]
                        , Html.button [ HtmlE.onClick <| go { def | type_ = Game.Quadruples, short = True } ] [ Html.text "Super (short)" ]
                        , Html.button [ HtmlE.onClick <| go { def | type_ = Game.Quadruples, multi = True } ] [ Html.text "Super (multiplayer)" ]
                        ]

                Just d ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| go d ] [ Html.text "Play!" ] ]
            ]
