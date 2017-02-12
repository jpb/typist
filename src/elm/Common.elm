module Common exposing (..)

import Task


cmd : a -> Cmd a
cmd a =
    Task.perform identity (Task.succeed a)


updateMap : (model -> ( model, Cmd msg )) -> ( model, Cmd msg ) -> ( model, Cmd msg )
updateMap f ( model, cmd ) =
    let
        ( model_, cmd_ ) =
            (f model)
    in
        ( model_, Cmd.batch [ cmd, cmd_ ] )
