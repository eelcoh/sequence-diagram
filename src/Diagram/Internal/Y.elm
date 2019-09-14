module Diagram.Internal.Y exposing
    ( add
    , compare
    , toFloat
    , toInt
    , toString
    )

import Diagram.Internal.Types exposing (Y(..))


compare : Y -> Y -> Order
compare (Y y1) (Y y2) =
    Basics.compare y1 y2


add : Y -> Y -> Y
add (Y y1) (Y y2) =
    Y (y1 + y2)


toString : Y -> String
toString (Y y) =
    "Y" ++ Basics.toString y


toInt : Y -> Int
toInt (Y y) =
    y


toFloat : Y -> Float
toFloat (Y y) =
    Basics.toFloat y
