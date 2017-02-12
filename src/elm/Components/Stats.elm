module Components.Stats exposing (..)

import Html exposing (Html, Attribute, div, input, text, program, p, button, h1, h2, table, tr, td, i)
import Metrics exposing (formatTime, calculateCharsPerMinute, calculateAccuracy)


type Msg
    = LoadHistory (List History)
    | AppendHistory History


type alias History =
    { repoName : String
    , repoBranch : String
    , filePath : String
    , elapsedTime : Float
    , charCount : Int
    , errorCount : Int
    }


type alias Model =
    { history : List History
    }


init : Model
init =
    { history = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AppendHistory history ->
            ( { model | history = history :: model.history }, Cmd.none )

        LoadHistory history ->
            ( { model | history = history }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "History" ]
        , table []
            (List.concat
                [ [ tr []
                        [ td [] [ text "Project" ]
                        , td [] [ text "File" ]
                        , td [] [ text "Chars/Minute" ]
                        , td [] [ text "Accuracy" ]
                        , td [] [ text "Time" ]
                        ]
                  ]
                , (List.map
                    (\history ->
                        tr []
                            [ td [] [ text history.repoName ]
                            , td [] [ text history.filePath ]
                            , td []
                                [ (calculateCharsPerMinute history.elapsedTime history.charCount)
                                    |> toString
                                    |> text
                                ]
                            , td []
                                [ text ((toString (calculateAccuracy history.charCount history.errorCount)) ++ "%") ]
                            , td [] [ text (formatTime history.elapsedTime) ]
                            ]
                    )
                    model.history
                  )
                ]
            )
        ]
