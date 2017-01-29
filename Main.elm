module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, program, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import String
import Json.Decode as Json
import Keyboard
import Set exposing (Set)
import Tutor


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs (\i -> TutorMsg (Tutor.KeyDown i))
        , Keyboard.ups (\i -> TutorMsg (Tutor.KeyUp i))
        , Keyboard.presses (\i -> TutorMsg (Tutor.KeyPress i))
        ]



-- MODEL


type alias Model =
    { tutor : Tutor.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { tutor =
            { content = ""
            , index = 0
            , text = "Here's to the crazy ones, the misfits"
            , errors = 0
            , keysPressed = Set.empty
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TutorMsg Tutor.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TutorMsg tutorMsg ->
            ( { model | tutor = (Tutor.update tutorMsg model.tutor) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (Html.map TutorMsg (Tutor.view model.tutor)) ]
