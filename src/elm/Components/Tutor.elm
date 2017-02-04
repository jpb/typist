module Components.Tutor exposing (..)

import Array exposing (Array)
import Base64
import Char
import Css exposing (backgroundColor)
import Css.Colors exposing (red)
import Dom
import Html exposing (Html, Attribute, div, input, text, p, strong, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode as Json
import KeyboardWithLocation as Keyboard
import Regex
import Set exposing (Set)
import String
import Task
import Time exposing (Time)
import Tuple


rightShiftChars =
    Set.fromList [ 'Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '~', '!', '@', '#', '$', '%' ]


leftShiftChars =
    Set.fromList [ 'Y', 'U', 'I', 'O', 'P', 'H', 'J', 'K', 'L', 'N', 'M', '^', '&', '*', '(', ')', '_', '+', '{', '}', '|', ':', '"', '<', '>', '?' ]


type State
    = Initial
    | Running
    | Paused
    | Finished


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Running then
        Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            , Keyboard.presses KeyPress
            , Time.every Time.second Tick
            ]
    else
        Sub.none


init : Model
init =
    { content = ""
    , lineIndex = 0
    , charIndex = 0
    , lines = Array.empty
    , errors = 0
    , keysPressed = Set.empty
    , state = Initial
    , file = File "" ""
    , elapsedTime = 0
    , loading = False
    }


type alias File =
    { path : String
    , content : String
    }


type alias Model =
    { content : String
    , lines : Array String
    , lineIndex : Int
    , charIndex : Int
    , errors : Int
    , keysPressed : Set ( Int, Int )
    , state : State
    , file : File
    , elapsedTime : Time
    , loading : Bool
    }


type Msg
    = KeyDown Keyboard.KeyCoordinates
    | KeyUp Keyboard.KeyCoordinates
    | KeyPress Keyboard.KeyCoordinates
    | SetFile String String
    | LoadContent (Result Http.Error String)
    | Tick Time
    | Start
    | Pause
    | Resume
    | Reset
    | Blurred (Result Dom.Error ())


type KeyPressResult
    = AdvanceChar
    | AdvanceLine
    | Error
    | Complete
    | Noop


keyPressResult : Model -> ( Int, Int ) -> KeyPressResult
keyPressResult model keyCoordinates =
    Maybe.withDefault Noop
        (Maybe.map
            (\currentLine ->
                let
                    current =
                        String.slice model.charIndex (model.charIndex + 1) currentLine

                    char =
                        (Char.fromCode (Tuple.first keyCoordinates))

                    isNewLine =
                        model.charIndex == String.length currentLine

                    isLastLine =
                        (model.lineIndex + 1) == Array.length model.lines

                    isLastChar =
                        (model.charIndex + 1) == String.length currentLine

                    isCurrentChar =
                        current == String.fromChar char
                in
                    if isNewLine then
                        if (Tuple.first keyCoordinates) == 13 then
                            AdvanceLine
                        else
                            Error
                    else if isCurrentChar then
                        if (Set.member char leftShiftChars) then
                            if (Set.member ( 16, 1 ) model.keysPressed) then
                                if isLastChar && isLastLine then
                                    Complete
                                else
                                    AdvanceChar
                            else
                                Error
                        else if Set.member char rightShiftChars then
                            if Set.member ( 16, 2 ) model.keysPressed then
                                if isLastChar && isLastLine then
                                    Complete
                                else
                                    AdvanceChar
                            else
                                Error
                        else if isLastChar && isLastLine then
                            Complete
                        else
                            AdvanceChar
                    else
                        Error
            )
            (Array.get model.lineIndex model.lines)
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Blurred _ ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | state = Initial
                , elapsedTime = 0
                , lineIndex = 0
                , charIndex = 0
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | elapsedTime = model.elapsedTime + Time.second }, Cmd.none )

        Start ->
            ( { model | state = Running }
            , Task.attempt Blurred (Dom.blur "tutor-action-button")
            )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Resume ->
            ( { model | state = Running }, Cmd.none )

        SetFile path url ->
            ( { model | file = (File path url), loading = True }, fetchContent url )

        LoadContent response ->
            case response of
                Ok base64Content ->
                    case (Base64.decode (Regex.replace Regex.All (Regex.regex "\\n") (\_ -> "") base64Content)) of
                        Ok content ->
                            ( { model | lines = (Array.fromList (String.lines content)), loading = False }, Cmd.none )

                        Err _ ->
                            ( { model | loading = False }, Cmd.none )

                Err _ ->
                    ( { model | loading = False }, Cmd.none )

        KeyDown keyCoordinates ->
            ( { model | keysPressed = Set.insert keyCoordinates model.keysPressed }, Cmd.none )

        KeyUp keyCoordinates ->
            ( { model | keysPressed = Set.remove keyCoordinates model.keysPressed }, Cmd.none )

        KeyPress keyCoordinates ->
            (case keyPressResult model keyCoordinates of
                AdvanceChar ->
                    { model | charIndex = model.charIndex + 1 }

                AdvanceLine ->
                    { model | lineIndex = model.lineIndex + 1, charIndex = 0 }

                Error ->
                    { model | errors = model.errors + 1 }

                Complete ->
                    { model | lineIndex = model.lineIndex + 1, charIndex = 0, state = Finished }

                Noop ->
                    model
            )
                ! []


fetchContent : String -> Cmd Msg
fetchContent url =
    Http.send LoadContent
        (Http.get url
            (Decode.at [ "content" ] Decode.string)
        )


actionButton model =
    case model.state of
        Initial ->
            [ button [ onClick Start, id "tutor-action-button" ] [ text "Start" ] ]

        Running ->
            [ button [ onClick Pause, id "tutor-action-button" ] [ text "Pause" ] ]

        Paused ->
            [ button [ onClick Resume, id "tutor-action-button" ] [ text "Resume" ] ]

        Finished ->
            [ text "Done!" ]


formatTime : Time -> String
formatTime floatTime =
    let
        intTime =
            (truncate (Time.inSeconds floatTime))

        hours =
            intTime // 3600

        minutes =
            (intTime - (hours * 3600)) // 60

        seconds =
            (intTime - (hours * 3600) - (minutes * 60))
    in
        (String.padLeft 2 '0' (toString hours))
            ++ ":"
            ++ (String.padLeft 2 '0' (toString minutes))
            ++ ":"
            ++ (String.padLeft 2 '0' (toString seconds))


styles =
    Css.asPairs >> Html.Attributes.style


view : Model -> Html Msg
view model =
    if model.loading then
        div [ class "row" ]
            [ p [] [ text "Loading..." ]
            ]
    else
        let
            currentLine =
                Maybe.withDefault "" (Array.get model.lineIndex model.lines)

            previousLines =
                Array.toList (Array.slice 0 model.lineIndex model.lines)

            pendingLines =
                Array.toList (Array.slice (model.lineIndex + 1) (Array.length model.lines) model.lines)

            previousChars =
                String.slice 0 model.charIndex currentLine

            currentChar =
                String.slice model.charIndex (model.charIndex + 1) currentLine

            pendingChars =
                String.slice (model.charIndex + 1) (String.length currentLine) currentLine
        in
            div [ class "row" ]
                (List.concat
                    [ [ p [] [ text "Errors: ", text (toString model.errors) ] ]
                    , actionButton model
                    , [ button [ onClick Reset ] [ text "Reset" ] ]
                    , [ text (formatTime model.elapsedTime) ]
                    , List.map (\l -> p [] [ text l ]) previousLines
                    , [ p []
                            [ text previousChars
                            , strong [ styles [ backgroundColor red ] ] [ text currentChar ]
                            , text pendingChars
                            ]
                      ]
                    , List.map (\l -> p [] [ text l ]) pendingLines
                    ]
                )
