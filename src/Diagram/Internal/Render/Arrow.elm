module Diagram.Internal.Render.Arrow exposing (view)

import Diagram.Internal.Attribute as Attributes
import Diagram.Internal.Render.Colour as Colour
import Diagram.Internal.Render.Line as Line
import Diagram.Internal.Render.Point as Point
import Diagram.Internal.Types exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as SvgA


view : Config -> Show -> Arrow -> Svg msg
view config active (Arrow attrs arrowDetails) =
    let
        lineCoords =
            calcLine config arrowDetails

        lineType =
            case arrowDetails.arrowType of
                ReturnArrow ->
                    Dashed

                _ ->
                    Full

        arrowLine =
            Line lineCoords lineType attrs active
                |> Line.draw config
                |> Just

        arrowHead =
            viewArrowHead config active attrs arrowDetails lineCoords.end
                |> Just

        arrowCaption =
            Maybe.map
                (viewArrowCaption config active attrs arrowDetails lineCoords.start lineCoords.end)
                (caption attrs arrowDetails)

        figures =
            List.filterMap identity [ arrowLine, arrowHead, arrowCaption ]

        anchor =
            case arrowDetails.direction of
                ToSelf ->
                    "left"

                _ ->
                    "middle"

        attributes =
            [ SvgA.textAnchor anchor ]
    in
    Svg.g [ SvgA.textAnchor anchor ]
        figures


viewArrowCaption : Config -> Show -> List Attribute -> ArrowDetails -> Point -> Point -> String -> Svg msg
viewArrowCaption config active attributes { direction } (Point x1 y1) (Point x2 y2) cpt =
    let
        ty =
            ((y1 + y2) / 2)
                + yOffset
                |> String.fromFloat

        yOffset =
            case direction of
                ToSelf ->
                    0

                _ ->
                    -0.5

        tx =
            (x1 + x2)
                / 2
                |> String.fromFloat

        textColor =
            Colour.txt active

        attrs =
            [ SvgA.y ty
            , SvgA.x tx
            , SvgA.fontFamily "Monospace"
            , SvgA.fontSize "6"
            , textColor
            ]
    in
    Svg.text_
        attrs
        [ Svg.text cpt ]


viewArrowHead : Config -> Show -> List Attribute -> ArrowDetails -> Point -> Svg msg
viewArrowHead config active attributes { arrowType, direction } (Point x y) =
    let
        szX =
            config.unitH / 2.0

        szY =
            config.unitV / 6.0

        y1 =
            y - szY

        y2 =
            y + szY

        x_ =
            case direction of
                LeftToRight ->
                    x - szX

                RightToLeft ->
                    x + szX

                ToSelf ->
                    x + szX

        pt ( a, b ) =
            String.join "," [ String.fromFloat a, String.fromFloat b ]

        pts =
            List.map pt [ ( x_, y1 ), ( x, y ), ( x_, y2 ) ]
                |> String.join " "

        ( figure, fill ) =
            case arrowType of
                SyncArrow ->
                    ( Svg.polygon, Colour.head active )

                _ ->
                    ( Svg.polyline, Colour.none )

        bgColor =
            Colour.fill active

        lineColor =
            Colour.stroke active

        attrs =
            [ SvgA.points pts
            , SvgA.strokeWidth "0.5"
            , lineColor
            , fill
            ]
    in
    figure
        attrs
        []



-- helper functions


calcLine : Config -> ArrowDetails -> LineCoordinates
calcLine ({ width, unitV, unitH, layerOffset } as config) { start, end, arrowType, direction } =
    case direction of
        LeftToRight ->
            { start = Point.point config start RightSide
            , between = Nothing
            , end = Point.point config end LeftSide
            }

        RightToLeft ->
            { start = Point.point config start LeftSide
            , between = Nothing
            , end = Point.point config end RightSide
            }

        ToSelf ->
            let
                between =
                    case arrowType of
                        ReturnArrow ->
                            ( Point.point config start SelfDown
                            , Point.point config end SelfUp
                            )

                        _ ->
                            ( Point.point config start SelfUp
                            , Point.point config end SelfDown
                            )
            in
            { start = Point.point config start RightSide
            , between = Just between
            , end = Point.point config end RightSide
            }


caption : List Attribute -> ArrowDetails -> Maybe String
caption attrs { arrowType } =
    case arrowType of
        ReturnArrow ->
            Attributes.getReturnCaption attrs

        _ ->
            Attributes.getCaption attrs



-- end
