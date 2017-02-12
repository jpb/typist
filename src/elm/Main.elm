port module Main exposing (..)

import Components.FileSearch as FileSearch
import Components.RepoSearch as RepoSearch
import Components.Tutor as Tutor
import Components.Stats as Stats
import Dom
import Html exposing (Html, Attribute, div, input, text, program, p, button, h1, h2, table, tr, td, i)
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
    let
        subs =
            case model.stage of
                Tutor ->
                    Sub.map TutorMsg (Tutor.subscriptions model.tutor)

                RepoSearch ->
                    Sub.map RepoSearchMsg (RepoSearch.subscriptions model.repoSearch)

                FileSearch ->
                    Sub.map FileSearchMsg (FileSearch.subscriptions model.fileSearch)

                Stats ->
                    Sub.batch []
    in
        Sub.batch
            [ subs
            , history LoadHistory
            ]


port appendHistory : Stats.History -> Cmd msg


port history : (List Stats.History -> msg) -> Sub msg


type Stage
    = RepoSearch
    | FileSearch
    | Tutor
    | Stats


type alias Model =
    { tutor : Tutor.Model
    , repoSearch : RepoSearch.Model
    , fileSearch : FileSearch.Model
    , stats : Stats.Model
    , stage : Stage
    , history : List Stats.History
    }


init : ( Model, Cmd Msg )
init =
    ( { tutor = Tutor.init
      , repoSearch = RepoSearch.init
      , fileSearch = FileSearch.init
      , stats = Stats.init
      , stage = RepoSearch
      , history = []
      }
    , Cmd.none
    )


type Msg
    = TutorMsg Tutor.Msg
    | RepoSearchMsg RepoSearch.Msg
    | FileSearchMsg FileSearch.Msg
    | StatsMsg Stats.Msg
    | NavigateTo Stage
    | Focused (Result Dom.Error ())
    | LoadHistory (List Stats.History)


focusOn : String -> Cmd Msg
focusOn id =
    Task.attempt Focused (Dom.focus id)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadHistory history ->
            let
                ( stats, statsCmds ) =
                    Stats.update (Stats.LoadHistory history) model.stats
            in
                ( { model | stats = stats }, Cmd.map StatsMsg statsCmds )

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
            case tutorMsg of
                Tutor.Completed elapsedTime charCount errorCount ->
                    case Maybe.map2 (,) model.repoSearch.selectedRepo model.fileSearch.selectedFile of
                        Just ( repo, file ) ->
                            let
                                history =
                                    { repoName = repo.name
                                    , repoBranch = repo.branch
                                    , filePath = file.path
                                    , elapsedTime = elapsedTime
                                    , charCount = charCount
                                    , errorCount = errorCount
                                    }

                                ( stats, statsCmd ) =
                                    Stats.update (Stats.AppendHistory history) model.stats
                            in
                                ( { model | stats = stats }
                                , Cmd.batch [ Cmd.map StatsMsg statsCmd, appendHistory history ]
                                )

                        _ ->
                            ( model, Cmd.none )

                _ ->
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

        StatsMsg statsMsg ->
            let
                ( stats, codeCmds ) =
                    Stats.update statsMsg model.stats
            in
                ( { model | stats = stats }, Cmd.map StatsMsg codeCmds )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        (List.concat
            [ [ div [ class "row" ]
                    [ i [ class "header--logo" ] []
                    , h1 [ class "header--title" ] [ text "Typist" ]
                    ]
              ]
            , (case model.stage of
                RepoSearch ->
                    [ button [ onClick (NavigateTo Stats) ] [ text "Stats" ]
                    , div [ class "row" ]
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

                Stats ->
                    [ button [ onClick (NavigateTo RepoSearch) ] [ text "↩" ]
                    , (Html.map StatsMsg (Stats.view model.stats))
                    ]
              )
            ]
        )
