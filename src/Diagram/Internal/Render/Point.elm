module Diagram.Internal.Render.Point exposing (coordsToString, offset, point, pointToString)

import Diagram.Internal.Layer as Layer
import Diagram.Internal.Render.Config exposing (calculateBase)
import Diagram.Internal.Types exposing (Config, Coordinate, LayerIdx, LifelineIdx, Point(..), Rectangle, Side(..), Y)
import Diagram.Internal.Y as Y


pointToString : Point -> String
pointToString (Point x1 y1) =
    let
        x =
            String.fromFloat x1

        y =
            String.fromFloat y1
    in
    String.join "," [ x, y ]


coordsToString : Point -> ( String, String )
coordsToString (Point x y) =
    ( String.fromFloat x, String.fromFloat y )


offset : Config -> Side -> Float
offset { unitH, layerOffset } side =
    case side of
        RightSide ->
            0.5 * unitH

        LeftSide ->
            -0.5 * unitH

        SelfUp ->
            2.5 * unitH

        SelfDown ->
            2.5 * unitH - layerOffset

        NoOffset ->
            0.0


point : Config -> Coordinate -> Side -> Point
point ({ unitV, unitH, layerOffset } as config) { lifelineIdx, layerIdx, y } side =
    let
        xLifeline =
            calculateBase config lifelineIdx

        offsetH =
            offset config side

        xLayer =
            Layer.toFloat layerIdx * layerOffset

        px =
            xLifeline + xLayer + offsetH

        py =
            Y.toFloat y * unitV
    in
    Point px py



--
