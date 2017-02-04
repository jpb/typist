module Components.FileSearch exposing (..)

import Autocomplete
import Common exposing (cmd)
import Html exposing (Html, Attribute, div, input, text, p, strong, ul, li)
import Html.Attributes exposing (class, classList, defaultValue, autofocus, id)
import Html.Events exposing (onInput, onFocus, onBlur)
import Http
import Json.Decode as Decode


init : Model
init =
    { query = ""
    , file = Nothing
    , tree = []
    , autocomplete = Autocomplete.empty
    , focused = False
    , selectedFile = Nothing
    , repo = Nothing
    , loading = False
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.selectedFile of
        Nothing ->
            if model.focused then
                Sub.map AutocompleteUpdate Autocomplete.subscription
            else
                Sub.none

        Just _ ->
            Sub.none


type alias Model =
    { query : String
    , tree : List File
    , file : Maybe File
    , autocomplete : Autocomplete.State
    , focused : Bool
    , selectedFile : Maybe File
    , repo : Maybe String
    , loading : Bool
    }


type alias File =
    { path : String
    , url : String
    }


type Msg
    = QueryChanged String
    | AutocompleteUpdate Autocomplete.Msg
    | LoadTree (Result Http.Error (List File))
    | SelectFile String
    | FileSelected File
    | SetRepo String String
    | Focus
    | Blur


fetchTree : String -> String -> Cmd Msg
fetchTree repo branch =
    Http.send LoadTree
        (Http.get
            ("https://api.github.com/repos/"
                ++ repo
                ++ "/git/trees/"
                ++ branch
                ++ "?recursive=1"
            )
            decodeTree
        )


decodeTree : Decode.Decoder (List File)
decodeTree =
    let
        decodeFile =
            Decode.map2 (\p u -> { path = p, url = u })
                (Decode.field "path" Decode.string)
                (Decode.field "url" Decode.string)
    in
        Decode.at [ "tree" ] (Decode.list decodeFile)


updateConfig : Autocomplete.UpdateConfig Msg File
updateConfig =
    Autocomplete.updateConfig
        { toId = .path
        , onKeyDown =
            \file maybeId ->
                if file == 13 then
                    Maybe.map SelectFile maybeId
                else
                    Nothing
        , onTooLow = Nothing
        , onTooHigh = Nothing
        , onMouseEnter = \id -> Nothing
        , onMouseLeave = \id -> Nothing
        , onMouseClick = \id -> Just <| SelectFile id
        , separateSelections = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRepo repo branch ->
            ( { model | repo = Just repo, loading = True }, fetchTree repo branch )

        QueryChanged query ->
            ( { model | query = query }, Cmd.none )

        SelectFile filePath ->
            let
                selectedFile =
                    (List.head (List.filter (\n -> n.path == filePath) model.tree))
            in
                case selectedFile of
                    Just file ->
                        ( { model | selectedFile = selectedFile }, cmd (FileSelected file) )

                    Nothing ->
                        ( model, Cmd.none )

        FileSelected _ ->
            ( model, Cmd.none )

        LoadTree response ->
            case response of
                Ok tree ->
                    ( { model | tree = tree, loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        AutocompleteUpdate autocompleteMsg ->
            let
                ( newState, maybeMsg ) =
                    Autocomplete.update updateConfig autocompleteMsg 5 model.autocomplete model.tree

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


viewConfig : Autocomplete.ViewConfig File
viewConfig =
    let
        customizedLi keySelected mouseSelected file =
            { attributes = [ classList [ ( "autocomplete-item", True ), ( "is-selected", keySelected || mouseSelected ) ] ]
            , children = [ text file.path ]
            }
    in
        Autocomplete.viewConfig
            { toId = .path
            , ul =
                [ class "autocomplete-list" ]
            , li =
                customizedLi
            }


filterTree : String -> List File -> List File
filterTree query tree =
    let
        lowerQuery =
            String.toLower query
    in
        List.filter (String.contains lowerQuery << String.toLower << .path) tree


view : Model -> Html Msg
view model =
    case model.repo of
        Just repo ->
            div []
                [ p [] [ text ("Search for a file in " ++ repo ++ "...") ]
                , input
                    [ onInput QueryChanged
                    , onFocus Focus
                    , onBlur Blur
                    , defaultValue model.query
                    , autofocus True
                    , id "file-search-query"
                    ]
                    []
                , Html.map
                    AutocompleteUpdate
                    (Autocomplete.view
                        viewConfig
                        5
                        model.autocomplete
                        (filterTree model.query model.tree)
                    )
                ]

        Nothing ->
            div [] []
