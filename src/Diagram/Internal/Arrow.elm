module Diagram.Internal.Arrow exposing (yEnd)

import Diagram.Internal.Compile.Coordinate as Coordinate
import Diagram.Internal.Types exposing (Arrow(..))


yEnd : Arrow -> Int
yEnd (Arrow _ { end }) =
    Coordinate.y end
