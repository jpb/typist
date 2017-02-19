module Components.Tutor exposing (..)

import Array exposing (Array)
import Base64
import Char
import Common
import Components.Error as Error exposing (httpErrorToString)
import Dom
import Html exposing (Html, Attribute, div, input, text, p, strong, button, span, i)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onWithOptions, on, onBlur)
import Http
import Json.Decode as Decode
import Json.Decode as Json
import Metrics exposing (formatTime, calculateCharsPerMinute, calculateAccuracy)
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


type alias KeyCoordinates =
    ( Int, Int )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.state == Running then
        Time.every Time.second Tick
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
    , loadError = Nothing
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
    , loadError : Maybe String
    , error : Maybe Error
    }


type Msg
    = KeyDown KeyCoordinates
    | KeyUp KeyCoordinates
    | KeyPress KeyCoordinates
    | SetFile String String
    | LoadContent (Result Http.Error String)
    | Tick Time
    | Start
    | Pause
    | Resume
    | Restart
    | Reset
    | Completed Time Int Int
    | DismissError Int
    | FocusInput
    | InputFocused (Result Dom.Error ())
    | InputBlurred


type Error
    = WrongChar
    | ShouldUseLeftShift
    | ShouldUseRightShift


type KeyPressResult
    = AdvanceChar
    | AdvanceLine
    | Error Error
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
                        if isLastLine then
                            Complete
                        else if (Tuple.first keyCoordinates) == 13 then
                            AdvanceLine
                        else
                            Error WrongChar
                    else if isCurrentChar then
                        if (Set.member char leftShiftChars) then
                            if (Set.member ( 16, 1 ) model.keysPressed) then
                                AdvanceChar
                            else
                                Error ShouldUseLeftShift
                        else if Set.member char rightShiftChars then
                            if Set.member ( 16, 2 ) model.keysPressed then
                                AdvanceChar
                            else
                                Error ShouldUseRightShift
                        else
                            AdvanceChar
                    else
                        Error WrongChar
            )
            (Array.get model.lineIndex model.lines)
        )


dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list =
    case list of
        [] ->
            []

        x :: xs ->
            if (predicate x) then
                dropWhile predicate xs
            else
                list


skipWhitespace : Model -> Model
skipWhitespace model =
    (Maybe.map
        (\currentLine ->
            let
                rest =
                    String.slice model.charIndex (String.length currentLine) currentLine

                advanceChars =
                    dropWhile (\c -> c == ' ') (String.toList rest)
                        |> List.length
                        |> (\l -> (String.length rest) - l)
            in
                { model | charIndex = model.charIndex + advanceChars }
        )
        (Array.get model.lineIndex model.lines)
    )
        |> Maybe.withDefault model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Completed _ _ _ ->
            ( model, Cmd.none )

        FocusInput ->
            ( model, Task.attempt InputFocused (Dom.focus "tutor-input") )

        InputFocused _ ->
            ( model, Cmd.none )

        InputBlurred ->
            if model.state == Running then
                (update FocusInput model)
            else
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
            Common.updateMap (update FocusInput) (update Reset { model | state = Running })

        Start ->
            Common.updateMap (update FocusInput) ( { model | state = Running }, Cmd.none )

        Pause ->
            ( { model | state = Paused }, Cmd.none )

        Resume ->
            ( model, Cmd.none )
                |> (Common.updateMap (update FocusInput))
                |> (Common.updateMap (\m -> ( { m | state = Running }, Cmd.none )))

        SetFile path url ->
            ( { model | file = (File path url), loading = True }, fetchContent url )
                |> Common.updateMap (update Reset)

        LoadContent response ->
            case response of
                Ok base64Content ->
                    case
                        (Base64.decode
                            (Regex.replace Regex.All
                                (Regex.regex "\\n")
                                (\_ -> "")
                                base64Content
                            )
                        )
                    of
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
                        loadError =
                            httpErrorToString err
                    in
                        ( { model
                            | loading = False
                            , loadError = Just loadError
                          }
                        , Cmd.none
                        )

        DismissError errorCount ->
            if errorCount == model.errorCount then
                ( { model | flashError = False, error = Nothing }, Cmd.none )
            else
                ( model, Cmd.none )

        KeyDown keyCoordinates ->
            let
                ( keyCode, _ ) =
                    keyCoordinates

                keysPressed_ =
                    Set.insert keyCoordinates model.keysPressed
            in
                if keyCode == 9 then
                    -- tab
                    let
                        model_ =
                            (skipWhitespace model)
                    in
                        ( { model_ | keysPressed = keysPressed_ }, Cmd.none )
                else
                    ( { model | keysPressed = keysPressed_ }, Cmd.none )

        KeyUp keyCoordinates ->
            ( { model | keysPressed = Set.remove keyCoordinates model.keysPressed }
            , Cmd.none
            )

        KeyPress keyCoordinates ->
            if model.state == Running then
                (case keyPressResult model keyCoordinates of
                    AdvanceChar ->
                        { model
                            | charIndex = model.charIndex + 1
                            , charCount = model.charCount + 1
                            , flashError = False
                        }
                            ! []

                    AdvanceLine ->
                        { model
                            | lineIndex = model.lineIndex + 1
                            , charIndex = 0
                            , charCount = model.charCount + 1
                        }
                            ! []

                    Error error ->
                        { model
                            | errorCount = model.errorCount + 1
                            , flashError = True
                            , error = Just error
                        }
                            ! [ Process.sleep (1000 * Time.millisecond)
                                    |> Task.perform
                                        (\_ ->
                                            DismissError
                                                (model.errorCount + 1)
                                        )
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
            else
                model ! []


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
            button
                [ onClick Start
                , id "tutor-action-button"
                , class "tutor-action-button--initial"
                ]
                [ text "Start" ]

        Running ->
            button [ onClick Pause, id "tutor-action-button" ] [ text "Pause" ]

        Paused ->
            button [ onClick Resume, id "tutor-action-button" ] [ text "Resume" ]

        Finished ->
            button [ onClick Restart, id "tutor-action-button" ] [ text "Restart" ]


keyCoordinates : Json.Decoder KeyCoordinates
keyCoordinates =
    Json.map2
        (,)
        (Json.field "keyCode" Json.int)
        (Json.field "location" Json.int)


errorMessage : Error -> List (Html Msg)
errorMessage error =
    case error of
        ShouldUseLeftShift ->
            [ text "Use "
            , i [ class "fa fa-chevron-left" ] []
            , text " shift"
            ]

        ShouldUseRightShift ->
            [ text "Use "
            , i [ class "fa fa-chevron-right" ] []
            , text " shift"
            ]

        WrongChar ->
            [ text "Wrong key!" ]


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

            showError =
                Maybe.map
                    (\e ->
                        case e of
                            ShouldUseLeftShift ->
                                True

                            ShouldUseRightShift ->
                                True

                            WrongChar ->
                                False
                    )
                    model.error
                    |> Maybe.withDefault False
        in
            case model.loadError of
                Just loadError ->
                    Error.view (Error.init loadError)

                Nothing ->
                    div []
                        [ div [ class "row tutor-nav" ]
                            [ actionButton model
                            , button [ onClick Reset ] [ text "Reset" ]
                            , span [] [ text (formatTime model.elapsedTime) ]
                            , span []
                                [ text
                                    ((toString
                                        (calculateCharsPerMinute model.elapsedTime
                                            model.charCount
                                        )
                                     )
                                        ++ " characters per minute"
                                    )
                                ]
                            , span []
                                [ text
                                    ((toString
                                        (calculateAccuracy model.charCount
                                            model.errorCount
                                        )
                                     )
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
                                                    , ( "tutor-active-char--error"
                                                      , model.flashError
                                                      )
                                                    ]
                                                ]
                                                [ text currentChar ]
                                            , input
                                                [ id "tutor-input"
                                                , (onWithOptions
                                                    "keydown"
                                                    { stopPropagation = False
                                                    , preventDefault = False
                                                    }
                                                    (Json.map KeyDown keyCoordinates)
                                                  )
                                                , (onWithOptions
                                                    "keypress"
                                                    { stopPropagation = False
                                                    , preventDefault = True
                                                    }
                                                    (Json.map KeyPress keyCoordinates)
                                                  )
                                                , (onWithOptions
                                                    "keyup"
                                                    { stopPropagation = False
                                                    , preventDefault = True
                                                    }
                                                    (Json.map KeyUp keyCoordinates)
                                                  )
                                                , onBlur InputBlurred
                                                ]
                                                []
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
                            , span
                                [ classList
                                    [ ( "tutor-error", True )
                                    , ( "tutor-error--show", showError )
                                    ]
                                ]
                                (Maybe.map errorMessage model.error |> Maybe.withDefault [])
                            ]
                        ]
