module Components.Error exposing (..)

import Html exposing (Html, div, text, p, strong)
import Html.Attributes exposing (class)


init : String -> Model
init text =
    { text = text }


type alias Model =
    { text : String
    }


view model =
    div [ class "error-text" ]
        [ strong [] [ text "Oh no! " ]
        , p [] [ text model.text ]
        ]
