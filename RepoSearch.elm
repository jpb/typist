module RepoSearch exposing (..)

import Html exposing (Html, Attribute, div, input, text, p, strong, ul, li)
import Html.Events exposing (onInput)
import Json.Decode as Decode
import Http


init : Model
init =
    { query = ""
    , repos = []
    }


type alias Model =
    { query : String
    , repos : List Repo
    }


type alias Repo =
    { name : String
    , url : String
    }


type Msg
    = QueryChanged String
    | LoadRepos (Result Http.Error (List Repo))
    | Select String


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QueryChanged query ->
            if String.length query > 2 then
                ( { model | query = query }, fetchRepos query )
            else
                ( model, Cmd.none )

        Select repoName ->
            ( model, Cmd.none )

        LoadRepos response ->
            case response of
                Ok repos ->
                    ( { model | repos = repos }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ [ input [ onInput QueryChanged ] [] ]
            , [ ul []
                    (List.map
                        (\r -> li [] [ text r.name ])
                        model.repos
                    )
              ]
            ]
        )
