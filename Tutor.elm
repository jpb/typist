module Tutor exposing (..)

import Html exposing (Html, Attribute, div, input, text, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import String
import Json.Decode as Json
import Keyboard
import Set exposing (Set)
import Char
import Tuple


rightShiftChars =
    Set.fromList [ 'Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '^', '&', '*', '(', ')', '_', '+' ]


leftShiftChars =
    Set.fromList [ 'Y', 'U', 'I', 'O', 'P', 'H', 'J', 'K', 'L', 'N', 'M', '~', '!', '@', '#', '$', '%', '{', '}', '|', ':', '"', '<', '>', '?' ]



-- MODEL


type alias Model =
    { content : String
    , text : String
    , index : Int
    , errors : Int
    , keysPressed : Set ( Int, Int )
    }



-- UPDATE


type Msg
    = KeyDown Keyboard.KeyCoordinates
    | KeyUp Keyboard.KeyCoordinates
    | KeyPress Keyboard.KeyCoordinates


isValidKeyPress : Model -> ( Int, Int ) -> Bool
isValidKeyPress model keyCoordinates =
    let
        current =
            String.slice model.index (model.index + 1) model.text

        char =
            (Char.fromCode (Tuple.first keyCoordinates))
    in
        if current == String.fromChar char then
            if Set.member char leftShiftChars then
                Set.member ( 16, 1 ) model.keysPressed
            else if Set.member char rightShiftChars then
                Set.member ( 16, 2 ) model.keysPressed
            else
                True
        else
            False


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown keyCoordinates ->
            { model | keysPressed = Set.insert keyCoordinates model.keysPressed }

        KeyUp keyCoordinates ->
            { model | keysPressed = Set.remove keyCoordinates model.keysPressed }

        KeyPress keyCoordinates ->
            if isValidKeyPress model keyCoordinates then
                { model | index = model.index + 1 }
            else
                { model | errors = model.errors + 1 }



-- VIEW


view : Model -> Html Msg
view model =
    let
        written =
            String.slice 0 model.index model.text

        current =
            String.slice model.index (model.index + 1) model.text

        pending =
            String.slice (model.index + 1) (String.length model.text) model.text
    in
        div []
            [ p [] [ text "Errors: ", text (toString model.errors) ]
            , p [] [ text written ]
            , p [] [ text current ]
            , p [] [ text pending ]
            ]
