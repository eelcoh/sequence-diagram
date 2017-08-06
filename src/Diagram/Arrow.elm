module Diagram.Arrow exposing (..)

import Diagram.Types exposing (Arrow(..))
import Diagram.Compile.Coordinate as Coordinate


yEnd : Arrow -> Int
yEnd (Arrow _ { end }) =
    Coordinate.y end
