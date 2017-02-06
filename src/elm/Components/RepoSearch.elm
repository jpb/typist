module Components.RepoSearch exposing (..)

import Autocomplete
import Common exposing (cmd)
import Debounce
import Html exposing (Html, Attribute, div, input, text, p, strong, ul, li, i)
import Html.Attributes exposing (class, classList, defaultValue, autofocus, id)
import Html.Events exposing (onInput, onFocus, onBlur)
import Http
import Json.Decode as Decode


init : Model
init =
    { query = ""
    , repos = []
    , autocomplete = Autocomplete.empty
    , focused = False
    , selectedRepo = Nothing
    , loading = False
    , debouncer = Debounce.init
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.focused then
        Sub.map AutocompleteUpdate Autocomplete.subscription
    else
        Sub.none


type alias Model =
    { query : String
    , repos : List Repo
    , autocomplete : Autocomplete.State
    , focused : Bool
    , selectedRepo : Maybe Repo
    , loading : Bool
    , debouncer : Debounce.State
    }


type alias Repo =
    { name : String
    , branch : String
    }


type Msg
    = QueryChanged String
    | AutocompleteUpdate Autocomplete.Msg
    | LoadRepos (Result Http.Error (List Repo))
    | SelectRepo String
    | RepoSelected Repo
    | Focus
    | Blur
    | DebounceMsg (Debounce.Msg Msg)


fetchRepos : String -> Cmd Msg
fetchRepos query =
    Http.send LoadRepos (Http.get ("https://api.github.com/search/repositories?q=" ++ query) decodeRepos)


decodeRepos : Decode.Decoder (List Repo)
decodeRepos =
    let
        decodeRepo =
            Decode.map2 (\n b -> { name = n, branch = b })
                (Decode.field "full_name" Decode.string)
                (Decode.field "default_branch" Decode.string)
    in
        Decode.at [ "items" ] (Decode.list decodeRepo)


updateConfig : Autocomplete.UpdateConfig Msg Repo
updateConfig =
    Autocomplete.updateConfig
        { toId = .name
        , onKeyDown =
            \code maybeId ->
                if code == 13 then
                    Maybe.map SelectRepo maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \id -> Nothing
        , onMouseLeave = \id -> Nothing
        , onMouseClick = \id -> Just <| SelectRepo id
        , separateSelections = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DebounceMsg a ->
            Debounce.update debounceConfig a model

        QueryChanged query ->
            case model.selectedRepo of
                Nothing ->
                    if query /= model.query && String.length query > 2 then
                        ( { model | query = query, loading = True }, fetchRepos query )
                    else
                        ( model, Cmd.none )

                Just _ ->
                    ( model, Cmd.none )

        SelectRepo repoName ->
            let
                selectedRepo =
                    (List.head (List.filter (\r -> r.name == repoName) model.repos))

                updatedModel =
                    { model | selectedRepo = selectedRepo }
            in
                case selectedRepo of
                    Just repo ->
                        ( updatedModel, cmd (RepoSelected repo) )

                    Nothing ->
                        ( updatedModel, Cmd.none )

        RepoSelected _ ->
            ( model, Cmd.none )

        LoadRepos response ->
            case response of
                Ok repos ->
                    ( { model | repos = repos, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        AutocompleteUpdate autocompleteMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autocompleteMsg 10 model.autocomplete model.repos

                newModel =
                    { model | autocomplete = newState }
            in
                case maybeMsg of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just updateMsg ->
                        update updateMsg newModel

        Focus ->
            ( { model | focused = True }, Cmd.none )

        Blur ->
            ( { model | focused = False }, Cmd.none )


viewConfig : Autocomplete.ViewConfig Repo
viewConfig =
    let
        customizedLi keySelected mouseSelected repo =
            { attributes =
                [ classList
                    [ ( "autocomplete-item", True )
                    , ( "autocomplete-item--selected", keySelected || mouseSelected )
                    ]
                ]
            , children = [ text repo.name ]
            }
    in
        Autocomplete.viewConfig
            { toId = .name
            , ul =
                [ class "autocomplete-list" ]
            , li =
                customizedLi
            }


debounceConfig : Debounce.Config { a | debouncer : Debounce.State } Msg
debounceConfig =
    (Debounce.config .debouncer
        (\model state -> { model | debouncer = state })
        DebounceMsg
        500
    )


debounce : (a -> Msg) -> (a -> Msg)
debounce =
    Debounce.debounce1 debounceConfig


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ input
            [ onInput (debounce QueryChanged)
            , onFocus Focus
            , onBlur Blur
            , defaultValue model.query
            , autofocus True
            , id "repo-search-query"
            , class "autocomplete-input"
            ]
            []
        , i
            [ classList
                [ ( "repo-search-spinner", True )
                , ( "repo-search-spinner--loading", model.loading )
                ]
            ]
            []
        , Html.map
            AutocompleteUpdate
            (Autocomplete.view
                viewConfig
                10
                model.autocomplete
                model.repos
            )
        ]
