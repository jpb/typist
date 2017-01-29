module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, program, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import String
import Json.Decode as Json
import Keyboard
import Set exposing (Set)
import Tutor
import RepoSearch
import Array


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
    , repoSearch : RepoSearch.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { tutor = Tutor.init "Here's to the crazy ones,\n the misfits"
      , repoSearch = RepoSearch.init
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TutorMsg Tutor.Msg
    | RepoSearchMsg RepoSearch.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TutorMsg tutorMsg ->
            ( { model | tutor = (Tutor.update tutorMsg model.tutor) }, Cmd.none )

        RepoSearchMsg repoSearchMsg ->
            let
                ( repoSearch, repoCmds ) =
                    RepoSearch.update repoSearchMsg model.repoSearch
            in
                ({ model | repoSearch = repoSearch }, Cmd.map RepoSearchMsg repoCmds)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (Html.map TutorMsg (Tutor.view model.tutor))
        , (Html.map RepoSearchMsg (RepoSearch.view model.repoSearch))
        ]
