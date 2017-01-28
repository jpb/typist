import Html exposing (Html, Attribute, div, input, text, program, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on)
import String
import Debug
import Json.Decode as Json
import Keyboard
import Set exposing (Set)
import Char
import Tuple

rightShiftChars = Set.fromList ['Q', 'W', 'R', 'E', 'T', 'A', 'S', 'D', 'F', 'G', 'Z', 'X', 'C', 'V', 'B', '^', '&', '*', '(', ')', '_', '+']
leftShiftChars = Set.fromList ['Y', 'U', 'I', 'O', 'P', 'H', 'J', 'K', 'L', 'N', 'M', '~', '!', '@', '#', '$', '%', '{', '}', '|', ':', '"', '<', '>', '?']

main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Keyboard.presses KeyPress
        ]


-- MODEL

type alias Model =
  { content : String
  , text : String
  , index : Int
  , errors : Int
  , keysPressed : Set (Int, Int)
  }

init : (Model, Cmd Msg)
init =
  ({ content = ""
   , index = 0
   , text = "Here's to the crazy ones, the misfits"
   , errors = 0
   , keysPressed = Set.empty }, Cmd.none)


-- UPDATE

type Msg
  = KeyDown Keyboard.KeyCoordinates
  | KeyUp Keyboard.KeyCoordinates
  | KeyPress Keyboard.KeyCoordinates

isValidKeyPress : Model -> (Int, Int) -> Bool
isValidKeyPress model keyCoordinates =
  let
    current = String.slice model.index (model.index + 1) model.text
    char = (Char.fromCode (Tuple.first keyCoordinates))
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

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown keyCoordinates ->
      ({ model | keysPressed = Set.insert keyCoordinates model.keysPressed }, Cmd.none)
    KeyUp keyCoordinates ->
      ({ model | keysPressed = Set.remove keyCoordinates model.keysPressed }, Cmd.none)
    KeyPress keyCoordinates ->
        if isValidKeyPress model keyCoordinates then
          ({ model | index = model.index + 1 }, Cmd.none)
        else
          ({ model | errors = model.errors + 1 }, Cmd.none)


-- VIEW

view : Model -> Html Msg
view model =
  let
    written = String.slice 0 model.index model.text
    current = String.slice model.index (model.index + 1) model.text
    pending = String.slice (model.index + 1) (String.length model.text) model.text
  in
    div []
      [ p [] [ text "Errors: ", text (toString model.errors) ]
      , p [] [ text written ]
      , p [] [ text current ]
      , p [] [ text pending ]
      ]
