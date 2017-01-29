module Main exposing (..)

import Html exposing (Html, Attribute, div, input, text, program, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import String
import Json.Decode as Json
import KeyboardWithLocation as Keyboard
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map TutorMsg (Tutor.subscriptions model.tutor)
        , Sub.map RepoSearchMsg (RepoSearch.subscriptions model.repoSearch)
        ]


type alias Model =
    { tutor : Tutor.Model
    , repoSearch : RepoSearch.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { tutor = Tutor.init "Here's to the crazy ones,\nthe misfits"
      , repoSearch = RepoSearch.init
      }
    , Cmd.none
    )


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
                ( { model | repoSearch = repoSearch }, Cmd.map RepoSearchMsg repoCmds )


view : Model -> Html Msg
view model =
    div []
        [ (Html.map TutorMsg (Tutor.view model.tutor))
        , (Html.map RepoSearchMsg (RepoSearch.view model.repoSearch))
        ]
