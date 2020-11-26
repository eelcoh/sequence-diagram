module Diagram.Internal.X exposing
    ( add
    , compare
    , toFloat
    , toInt
    , toString
    )

import Diagram.Internal.Types exposing (X(..))


compare : X -> X -> Order
compare (X y1) (X y2) =
    Basics.compare y1 y2


add : X -> X -> X
add (X y1) (X y2) =
    X (y1 + y2)


toString : X -> String
toString (X x) =
    "X" ++ String.fromInt x


toInt : X -> Int
toInt (X x) =
    x


toFloat : X -> Float
toFloat (X x) =
    Basics.toFloat x
