module Tutor exposing (..)

import Html exposing (Html, Attribute, div, input, text, p, strong)
import Html.Attributes exposing (..)
import String
import Json.Decode as Json
import Keyboard
import Set exposing (Set)
import Char
import Tuple
import Array exposing (Array)


rightShiftChars =
    Set.fromList [ 'Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '^', '&', '*', '(', ')', '_', '+' ]


leftShiftChars =
    Set.fromList [ 'Y', 'U', 'I', 'O', 'P', 'H', 'J', 'K', 'L', 'N', 'M', '~', '!', '@', '#', '$', '%', '{', '}', '|', ':', '"', '<', '>', '?' ]



-- MODEL


init : String -> Model
init text =
    { content = ""
    , lineIndex = 0
    , charIndex = 0
    , lines = Array.fromList (String.lines text)
    , errors = 0
    , keysPressed = Set.empty
    }


type alias Model =
    { content : String
    , lines : Array String
    , lineIndex : Int
    , charIndex : Int
    , errors : Int
    , keysPressed : Set ( Int, Int )
    }



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCoordinates
    | KeyUp Keyboard.KeyCoordinates
    | KeyPress Keyboard.KeyCoordinates


type KeyPressResult
    = AdvanceChar
    | AdvanceLine
    | Error
    | Noop


keyPressResult : Model -> ( Int, Int ) -> KeyPressResult
keyPressResult model keyCoordinates =
    Maybe.withDefault Noop
        (Maybe.map
            (\currentLine ->
                if model.charIndex == String.length currentLine then
                    if (Tuple.first keyCoordinates) == 13 then
                        -- Enter
                        AdvanceLine
                    else
                        Error
                else
                    let
                        current =
                            String.slice model.charIndex (model.charIndex + 1) currentLine

                        char =
                            (Char.fromCode (Tuple.first keyCoordinates))
                    in
                        if current == String.fromChar char then
                            if Set.member char leftShiftChars then
                                if Set.member ( 16, 1 ) model.keysPressed then
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


update : Msg -> Model -> Model
update msg model =
    case msg of
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

                Noop ->
                    model
            )



-- VIEW


view : Model -> Html Msg
view model =
    case Array.get model.lineIndex model.lines of
        Just currentLine ->
            let
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

        Nothing ->
            div [] [ text "Done!" ]
