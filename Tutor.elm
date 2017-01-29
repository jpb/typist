module Tutor exposing (..)

import Html exposing (Html, Attribute, div, input, text, p, strong, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import Json.Decode as Json
import KeyboardWithLocation as Keyboard
import Set exposing (Set)
import Char
import Tuple
import Array exposing (Array)


rightShiftChars =
    Set.fromList [ 'Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '^', '&', '*', '(', ')', '_', '+' ]


leftShiftChars =
    Set.fromList [ 'Y', 'U', 'I', 'O', 'P', 'H', 'J', 'K', 'L', 'N', 'M', '~', '!', '@', '#', '$', '%', '{', '}', '|', ':', '"', '<', '>', '?' ]


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
            ]
    else
        Sub.none


init : String -> Model
init text =
    { content = ""
    , lineIndex = 0
    , charIndex = 0
    , lines = Array.fromList (String.lines text)
    , errors = 0
    , keysPressed = Set.empty
    , state = Initial
    }


type alias Model =
    { content : String
    , lines : Array String
    , lineIndex : Int
    , charIndex : Int
    , errors : Int
    , keysPressed : Set ( Int, Int )
    , state : State
    }


type Msg
    = KeyDown Keyboard.KeyCoordinates
    | KeyUp Keyboard.KeyCoordinates
    | KeyPress Keyboard.KeyCoordinates
    | Start
    | Pause
    | Resume


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Start ->
            { model | state = Running }

        Pause ->
            { model | state = Paused }

        Resume ->
            { model | state = Running }

        KeyDown keyCoordinates ->
            { model | keysPressed = Set.insert keyCoordinates model.keysPressed }

        KeyUp keyCoordinates ->
            { model | keysPressed = Set.remove keyCoordinates model.keysPressed }

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


actionButton model =
    case model.state of
        Initial ->
            [ button [ onClick Start ] [ text "Start" ] ]

        Running ->
            [ button [ onClick Pause ] [ text "Pause" ] ]

        Paused ->
            [ button [ onClick Resume ] [ text "Resume" ] ]

        Finished ->
            [ text "Done!" ]


view : Model -> Html Msg
view model =
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
        div []
            (List.concat
                [ [ p [] [ text "Errors: ", text (toString model.errors) ] ]
                , actionButton model
                , List.map (\l -> p [] [ text l ]) previousLines
                , [ p []
                        [ text previousChars
                        , strong [] [ text currentChar ]
                        , text pendingChars
                        ]
                  ]
                , List.map (\l -> p [] [ text l ]) pendingLines
                ]
            )
