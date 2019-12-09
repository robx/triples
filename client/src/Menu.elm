module Menu exposing
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
    , game : Maybe Game.GameDef
    , name : Maybe String
    , style : Style.Style
    }


type Msg
    = ToggleDetails
    | NameChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleDetails ->
            { model | scoreDetails = not model.scoreDetails }

        NameChange n ->
            case n of
                "" ->
                    { model | name = Nothing }

                _ ->
                    { model | name = Just n }


view : (Msg -> msg) -> (Game.GameDef -> String -> msg) -> Model -> Html.Html msg
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
            case model.game of
                Nothing ->
                    "Choose a game"
                        ++ (case model.name of
                                Just n ->
                                    ", " ++ n ++ "!"

                                Nothing ->
                                    "!"
                           )

                Just d ->
                    "Welcome"
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

        gogo def =
            go def (Maybe.withDefault "nobody" model.name)

        name =
            Html.div
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd model.style.colors.symbols ) ] ]
                [ Html.span [] [ Html.text "Set your name (for multiplayer)" ]
                , Html.input [ HtmlA.value (Maybe.withDefault "" model.name), HtmlE.onInput <| wrap << NameChange ] []
                ]

        maybeName =
            case model.game of
                Nothing ->
                    [ name ]

                Just d ->
                    if d.multi then
                        [ name ]

                    else
                        []
    in
    Html.div [ HtmlA.id "menu" ] <|
        addScore <|
            [ Html.div
                [ HtmlA.class "msg", HtmlA.style [ ( "background", trd model.style.colors.symbols ) ] ]
                [ Html.text prompt ]
            , case model.game of
                Nothing ->
                    Html.div [ HtmlA.class "buttons" ] <|
                        [ Html.button [ HtmlE.onClick <| gogo def ] [ Html.text "Classic" ]
                        , Html.button [ HtmlE.onClick <| gogo { def | short = True } ] [ Html.text "Classic (short)" ]
                        , Html.button [ HtmlE.onClick <| gogo { def | multi = True }, HtmlA.disabled (model.name == Nothing) ] [ Html.text "Classic (multi)" ]
                        , Html.button [ HtmlE.onClick <| gogo { def | type_ = Game.Quadruples } ] [ Html.text "Super" ]
                        , Html.button [ HtmlE.onClick <| gogo { def | type_ = Game.Quadruples, short = True } ] [ Html.text "Super (short)" ]
                        , Html.button [ HtmlE.onClick <| gogo { def | type_ = Game.Quadruples, multi = True }, HtmlA.disabled (model.name == Nothing) ] [ Html.text "Super (multi)" ]
                        ]

                Just d ->
                    Html.div [ HtmlA.class "button" ] <|
                        [ Html.button [ HtmlE.onClick <| gogo d ] [ Html.text "Play!" ] ]
            ]
                ++ maybeName
