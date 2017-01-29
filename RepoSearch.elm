module RepoSearch exposing (..)

import Html exposing (Html, Attribute, div, input, text, p, strong, ul, li)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onInput, onFocus, onBlur)
import Json.Decode as Decode
import Http
import Autocomplete


init : Model
init =
    { query = ""
    , repos = []
    , autocomplete = Autocomplete.empty
    , focused = False
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
    }


type alias Repo =
    { name : String
    , url : String
    }


type Msg
    = QueryChanged String
    | AutocompleteUpdate Autocomplete.Msg
    | LoadRepos (Result Http.Error (List Repo))
    | SelectRepo String
    | Focus
    | Blur


fetchRepos : String -> Cmd Msg
fetchRepos query =
    Http.send LoadRepos (Http.get ("https://api.github.com/search/repositories?q=" ++ query) decodeRepos)


decodeRepos : Decode.Decoder (List Repo)
decodeRepos =
    let
        decodeRepo =
            Decode.map2 (\n u -> { name = n, url = u })
                (Decode.field "full_name" Decode.string)
                (Decode.field "html_url" Decode.string)
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
        , onMouseEnter = \_ -> Nothing
        , onMouseLeave = \_ -> Nothing
        , onMouseClick = \id -> Just <| SelectRepo id
        , separateSelections = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged query ->
            if query /= model.query && String.length query > 2 then
                ( { model | query = query }, fetchRepos query )
            else
                ( model, Cmd.none )

        SelectRepo repoName ->
            ( model, Cmd.none )

        LoadRepos response ->
            case response of
                Ok repos ->
                    ( { model | repos = repos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AutocompleteUpdate autocompleteMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autocompleteMsg 5 model.autocomplete model.repos

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
            { attributes = [ classList [ ( "autocomplete-item", True ), ( "is-selected", keySelected || mouseSelected ) ] ]
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


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput QueryChanged, onFocus Focus, onBlur Blur ] []
        , Html.map
            AutocompleteUpdate
            (Autocomplete.view
                viewConfig
                5
                model.autocomplete
                model.repos
            )
        ]
