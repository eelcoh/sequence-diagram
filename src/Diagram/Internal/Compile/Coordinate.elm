module Diagram.Internal.Compile.Coordinate exposing (toCoordinate, y)

import Diagram.Internal.Types exposing (Config, Coordinate, LayerIdx, LifelineIdx, Point(..), Rectangle, Side(..), Y)
import Diagram.Internal.Y as Y


toCoordinate : LifelineIdx -> LayerIdx -> Y -> Coordinate
toCoordinate lifelineIdx layerIdx y =
    { lifelineIdx = lifelineIdx, layerIdx = layerIdx, y = y }


y : Coordinate -> Int
y { y } =
    Y.toInt y



--
