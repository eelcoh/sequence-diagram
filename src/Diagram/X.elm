module Diagram.X
    exposing
        ( compare
        , add
        , toString
        , toInt
        , toFloat
        )

import Diagram.Types exposing (X(..))


compare : X -> X -> Order
compare (X y1) (X y2) =
    Basics.compare y1 y2


add : X -> X -> X
add (X y1) (X y2) =
    X (y1 + y2)


toString : X -> String
toString (X x) =
    "X" ++ (Basics.toString x)


toInt : X -> Int
toInt (X x) =
    x


toFloat : X -> Float
toFloat (X x) =
    Basics.toFloat x
