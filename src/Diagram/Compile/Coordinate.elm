module Diagram.Compile.Coordinate exposing (..)

import Diagram.Types exposing (Config, Coordinate, Point(..), LifelineIdx, LayerIdx, Y, Side(..), Rectangle)
import Diagram.Y as Y


toCoordinate : LifelineIdx -> LayerIdx -> Y -> Coordinate
toCoordinate lifelineIdx layerIdx y =
    { lifelineIdx = lifelineIdx, layerIdx = layerIdx, y = y }


y : Coordinate -> Int
y { y } =
    Y.toInt y



--
