module Diagram.Internal.Compile.Coordinate exposing (toCoordinate, y)

import Diagram.Internal.Types exposing (Coordinate, LayerIdx, LifelineIdx, Point(..), Side(..), Y)
import Diagram.Internal.Y as Y


toCoordinate : LifelineIdx -> LayerIdx -> Y -> Coordinate
toCoordinate lifelineIdx layerIdx yy =
    Coordinate lifelineIdx layerIdx yy


y : Coordinate -> Int
y c =
    Y.toInt c.y



--
