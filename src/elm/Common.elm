module Common exposing (..)

import Task


cmd : a -> Cmd a
cmd a =
    Task.perform identity (Task.succeed a)
