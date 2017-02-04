module Main exposing (..)

import Components.FileSearch as FileSearch
import Components.RepoSearch as RepoSearch
import Components.Tutor as Tutor
import Dom
import Html exposing (Html, Attribute, div, input, text, program, p, button, h1)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput, onClick)
import Task


main : Program Never Model Msg
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
        , Sub.map FileSearchMsg (FileSearch.subscriptions model.fileSearch)
        ]


type Stage
    = RepoSearch
    | FileSearch
    | Tutor


type alias Model =
    { tutor : Tutor.Model
    , repoSearch : RepoSearch.Model
    , fileSearch : FileSearch.Model
    , stage : Stage
    }


init : ( Model, Cmd Msg )
init =
    ( { tutor = Tutor.init
      , repoSearch = RepoSearch.init
      , fileSearch = FileSearch.init
      , stage = RepoSearch
      }
    , Cmd.none
    )


type Msg
    = TutorMsg Tutor.Msg
    | RepoSearchMsg RepoSearch.Msg
    | FileSearchMsg FileSearch.Msg
    | NavigateTo Stage
    | Focused (Result Dom.Error ())


focusOn : String -> Cmd Msg
focusOn id =
    Task.attempt Focused (Dom.focus id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Focused _ ->
            ( model, Cmd.none )

        NavigateTo stage ->
            let
                cmd =
                    case stage of
                        RepoSearch ->
                            focusOn "repo-search-query"

                        FileSearch ->
                            focusOn "file-search-query"

                        _ ->
                            Cmd.none
            in
                ( { model | stage = stage }, cmd )

        TutorMsg tutorMsg ->
            let
                ( updatedTutor, tutorCmd ) =
                    Tutor.update tutorMsg model.tutor
            in
                ( { model | tutor = updatedTutor }, Cmd.map TutorMsg tutorCmd )

        RepoSearchMsg repoSearchMsg ->
            case repoSearchMsg of
                RepoSearch.RepoSelected repo ->
                    let
                        ( updatedFileSearch, fileSearchCmd ) =
                            FileSearch.update (FileSearch.SetRepo repo.name repo.branch) model.fileSearch
                    in
                        ( { model
                            | stage = FileSearch
                            , fileSearch = updatedFileSearch
                          }
                        , (Cmd.batch
                            [ focusOn "file-search-query"
                            , Cmd.map FileSearchMsg fileSearchCmd
                            ]
                          )
                        )

                _ ->
                    let
                        ( repoSearch, repoSearchCmds ) =
                            RepoSearch.update repoSearchMsg model.repoSearch
                    in
                        ( { model | repoSearch = repoSearch }, Cmd.map RepoSearchMsg repoSearchCmds )

        FileSearchMsg fileSearchMsg ->
            case fileSearchMsg of
                FileSearch.FileSelected file ->
                    let
                        ( updatedTutor, tutorCmd ) =
                            Tutor.update (Tutor.SetFile file.path file.url) model.tutor
                    in
                        ( { model
                            | stage = Tutor
                            , tutor = updatedTutor
                          }
                        , Cmd.map TutorMsg tutorCmd
                        )

                _ ->
                    let
                        ( fileSearch, codeCmds ) =
                            FileSearch.update fileSearchMsg model.fileSearch
                    in
                        ( { model | fileSearch = fileSearch }, Cmd.map FileSearchMsg codeCmds )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        (List.concat
            [ [ div [ class "row" ]
                    [ h1 [] [ text "Typist" ] ]
              ]
            , (case model.stage of
                RepoSearch ->
                    [ div [ class "row" ]
                        [ p [] [ text "Search for a project on GitHub..." ] ]
                    , (Html.map RepoSearchMsg (RepoSearch.view model.repoSearch))
                    ]

                FileSearch ->
                    [ div [ class "row" ]
                        [ button [ onClick (NavigateTo RepoSearch) ] [ text "↩" ]
                        , p [] [ text "Search for a file..." ]
                        ]
                    , (Html.map FileSearchMsg (FileSearch.view model.fileSearch))
                    ]

                Tutor ->
                    [ button [ onClick (NavigateTo FileSearch) ] [ text "↩" ]
                    , (Html.map TutorMsg (Tutor.view model.tutor))
                    ]
              )
            ]
        )
