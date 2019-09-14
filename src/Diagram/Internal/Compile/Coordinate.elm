module Diagram.Internal.Compile.Coordinate exposing (toCoordinate, y)

import Diagram.Internal.Types exposing (Config, Coordinate, LayerIdx, LifelineIdx, Point(..), Rectangle, Side(..), Y)
import Diagram.Internal.Y as Y


toCoordinate : LifelineIdx -> LayerIdx -> Y -> Coordinate
toCoordinate lifelineIdx layerIdx y_ =
    { lifelineIdx = lifelineIdx, layerIdx = layerIdx, y = y_ }


y : Coordinate -> Int
y c =
    Y.toInt c.y



--
