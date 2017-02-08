module Components.Error exposing (..)

import Html exposing (Html, div, text, p, strong, i)
import Html.Attributes exposing (class)
import Http


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl _ ->
            "Bad Url"

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Status"

        Http.BadPayload _ response ->
            "Bad Payload"


init : String -> Model
init text =
    { text = text }


type alias Model =
    { text : String
    }


view model =
    div [ class "error-text" ]
        [ strong [] [ text "Oh no! " ]
        , i [] [ text model.text ]
        , text " :("
        , p [] [ text "Sorry about that, can you try again?" ]
        ]
