module Metrics exposing (..)

import Time exposing (Time)

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
