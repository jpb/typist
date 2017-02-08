module Components.Tutor exposing (..)

import Array exposing (Array)
import Base64
import Char
import Common
import Components.Error as Error exposing (httpErrorToString)
import Dom
import Html exposing (Html, Attribute, div, input, text, p, strong, button, span)
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
import Process


rightShiftChars : Set Char
rightShiftChars =
    Set.fromList [ 'Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '~', '!', '@', '#', '$', '%' ]


leftShiftChars : Set Char
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
    , errorCount = 0
    , keysPressed = Set.empty
    , state = Initial
    , file = File "" ""
    , elapsedTime = 0
    , loading = False
    , charCount = 0
    , flashError = False
    , error = Nothing
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
    , errorCount : Int
    , keysPressed : Set ( Int, Int )
    , state : State
    , file : File
    , elapsedTime : Time
    , loading : Bool
    , charCount : Int
    , flashError : Bool
    , error : Maybe String
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
    | Restart
    | Reset
    | Blurred (Result Dom.Error ())
    | Completed Time Int Int
    | DismissError


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
                    if isLastChar then
                        if isLastLine then
                            Complete
                        else
                            AdvanceChar
                    else if isNewLine then
                        if (Tuple.first keyCoordinates) == 13 then
                            AdvanceLine
                        else
                            Error
                    else if isCurrentChar then
                        if (Set.member char leftShiftChars) then
                            if (Set.member ( 16, 1 ) model.keysPressed) then
                                AdvanceChar
                            else
                                Error
                        else if Set.member char rightShiftChars then
                            if Set.member ( 16, 2 ) model.keysPressed then
                                AdvanceChar
                            else
                                Error
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
        Completed _ _ _ ->
            ( model, Cmd.none )

        Blurred _ ->
            ( model, Cmd.none )

        Reset ->
            ( { model
                | state = Initial
                , elapsedTime = 0
                , lineIndex = 0
                , charIndex = 0
                , charCount = 0
                , errorCount = 0
              }
            , Cmd.none
            )

        Tick _ ->
            ( { model | elapsedTime = model.elapsedTime + Time.second }, Cmd.none )

        Restart ->
            let
                ( updatedModel, cmds ) =
                    (update Reset { model | state = Running })
            in
                ( updatedModel
                , (Cmd.batch [ cmds, Task.attempt Blurred (Dom.blur "tutor-action-button") ])
                )

        Start ->
            ( { model | state = Running }
            , Task.attempt Blurred (Dom.blur "tutor-action-button")
            )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Resume ->
            ( { model | state = Running }
            , Task.attempt Blurred (Dom.blur "tutor-action-button")
            )

        SetFile path url ->
            let
                ( updatedModel, cmds ) =
                    (update Reset { model | file = (File path url), loading = True })
            in
                ( updatedModel
                , (Cmd.batch [ cmds, fetchContent url ])
                )

        LoadContent response ->
            case response of
                Ok base64Content ->
                    case (Base64.decode (Regex.replace Regex.All (Regex.regex "\\n") (\_ -> "") base64Content)) of
                        Ok content ->
                            ( { model
                                | lines =
                                    (Array.fromList (String.lines content))
                                , loading = False
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | loading = False }, Cmd.none )

                Err err ->
                    let
                        error =
                            httpErrorToString err
                    in
                        ( { model
                            | loading = False
                            , error = Just error
                          }
                        , Cmd.none
                        )

        DismissError ->
            ( { model | flashError = False }, Cmd.none )

        KeyDown keyCoordinates ->
            ( { model | keysPressed = Set.insert keyCoordinates model.keysPressed }, Cmd.none )

        KeyUp keyCoordinates ->
            ( { model | keysPressed = Set.remove keyCoordinates model.keysPressed }, Cmd.none )

        KeyPress keyCoordinates ->
            (case keyPressResult model keyCoordinates of
                AdvanceChar ->
                    { model
                        | charIndex = model.charIndex + 1
                        , charCount = model.charCount + 1
                    }
                        ! []

                AdvanceLine ->
                    { model
                        | lineIndex = model.lineIndex + 1
                        , charIndex = 0
                        , charCount = model.charCount + 1
                    }
                        ! []

                Error ->
                    { model
                        | errorCount = model.errorCount + 1
                        , flashError = True
                    }
                        ! [ Process.sleep (1000 * Time.millisecond)
                                |> Task.perform (\_ -> DismissError)
                          ]

                Complete ->
                    ( { model
                        | lineIndex = model.lineIndex + 1
                        , charIndex = 0
                        , state = Finished
                        , charCount = model.charCount + 1
                      }
                    , Common.cmd
                        (Completed model.elapsedTime
                            (model.charCount + 1)
                            model.errorCount
                        )
                    )

                Noop ->
                    model ! []
            )


fetchContent : String -> Cmd Msg
fetchContent url =
    Http.send LoadContent
        (Http.get url
            (Decode.at [ "content" ] Decode.string)
        )


actionButton : Model -> Html Msg
actionButton model =
    case model.state of
        Initial ->
            button [ onClick Start, id "tutor-action-button", class "tutor-action-button--initial" ] [ text "Start" ]

        Running ->
            button [ onClick Pause, id "tutor-action-button" ] [ text "Pause" ]

        Paused ->
            button [ onClick Resume, id "tutor-action-button" ] [ text "Resume" ]

        Finished ->
            button [ onClick Restart, id "tutor-action-button" ] [ text "Restart" ]


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
        if hours > 0 then
            (toString hours)
                ++ ":"
                ++ (String.padLeft 2 '0' (toString minutes))
                ++ ":"
                ++ (String.padLeft 2 '0' (toString seconds))
        else
            (String.padLeft 2 '0' (toString minutes))
                ++ ":"
                ++ (String.padLeft 2 '0' (toString seconds))


calculateCharsPerMinute : Time -> Int -> Int
calculateCharsPerMinute time charCount =
    (truncate ((Time.inSeconds time / (toFloat charCount)) * 60))


calculateAccuracy : Int -> Int -> Int
calculateAccuracy charCount errorCount =
    ((charCount - errorCount) // charCount) * 100


view : Model -> Html Msg
view model =
    if model.loading then
        div [ class "row" ]
            [ p [] [ text "Loading..." ]
            ]
    else
        let
            numberOfLines =
                Array.length model.lines

            lines =
                Array.indexedMap
                    (\i l ->
                        if i + 1 == numberOfLines then
                            l
                        else
                            l ++ "¶"
                    )
                    model.lines

            currentLine =
                Maybe.withDefault "" (Array.get model.lineIndex lines)

            previousLines =
                Array.toList (Array.slice 0 model.lineIndex lines)

            pendingLines =
                Array.toList
                    (Array.slice (model.lineIndex + 1)
                        (Array.length lines)
                        lines
                    )

            previousChars =
                String.slice 0 model.charIndex currentLine

            currentChar =
                String.slice model.charIndex (model.charIndex + 1) currentLine

            pendingChars =
                String.slice (model.charIndex + 1) (String.length currentLine) currentLine
        in
            case model.error of
                Just error ->
                    Error.view (Error.init error)

                Nothing ->
                    div []
                        [ div [ class "row tutor-nav" ]
                            [ actionButton model
                            , button [ onClick Reset ] [ text "Reset" ]
                            , span [] [ text (formatTime model.elapsedTime) ]
                            , span []
                                [ text
                                    ((toString (calculateCharsPerMinute model.elapsedTime model.charCount))
                                        ++ " characters per minute"
                                    )
                                ]
                            , span []
                                [ text
                                    ((toString (calculateAccuracy model.charCount model.errorCount))
                                        ++ "% accuracy"
                                    )
                                ]
                            ]
                        , div [ class "row" ]
                            [ div [ class "tutor-text" ]
                                (List.concat
                                    [ List.map
                                        (\l -> p [] [ text l ])
                                        previousLines
                                    , [ p []
                                            [ text previousChars
                                            , span
                                                [ classList
                                                    [ ( "tutor-active-char", True )
                                                    , ( "tutor-active-char--error", model.flashError )
                                                    ]
                                                ]
                                                [ text currentChar ]
                                            , text pendingChars
                                            ]
                                      ]
                                    , List.map
                                        (\l ->
                                            p []
                                                [ text
                                                    (if l == "" then
                                                        " "
                                                     else
                                                        l
                                                    )
                                                ]
                                        )
                                        pendingLines
                                    ]
                                )
                            ]
                        ]
