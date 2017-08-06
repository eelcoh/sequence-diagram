module Diagram.Render.Point exposing (..)

import Diagram.Layer as Layer
import Diagram.Render.Config exposing (calculateBase)
import Diagram.Types exposing (Config, Coordinate, LayerIdx, LifelineIdx, Point(..), Rectangle, Side(..), Y)
import Diagram.Y as Y


pointToString : Point -> String
pointToString (Point x1 y1) =
    let
        x =
            toString x1

        y =
            toString y1
    in
        String.join "," [ x, y ]


coordsToString : Point -> ( String, String )
coordsToString (Point x y) =
    ( (toString x), (toString y) )


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
point ({ width, unitV, unitH, layerOffset } as config) { lifelineIdx, layerIdx, y } side =
    let
        xLifeline =
            calculateBase config lifelineIdx

        offsetH =
            offset config side

        xLayer =
            (Layer.toFloat layerIdx) * layerOffset

        px =
            xLifeline + xLayer + offsetH

        py =
            (Y.toFloat y) * unitV
    in
        Point px py


stringify : Rectangle -> ( String, String, String, String )
stringify ( a, b, c, d ) =
    ( toString a, toString b, toString c, toString d )



--
