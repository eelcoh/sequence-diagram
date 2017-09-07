module Diagram.Internal.Arrow exposing (..)

import Diagram.Internal.Types exposing (Arrow(..))
import Diagram.Internal.Compile.Coordinate as Coordinate


yEnd : Arrow -> Int
yEnd (Arrow _ { end }) =
    Coordinate.y end
