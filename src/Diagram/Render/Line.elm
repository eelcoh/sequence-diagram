module Diagram.Render.Line exposing (draw)

import Svg
import Svg.Attributes as SvgA
import Diagram.Render.Point exposing (..)
import Diagram.Types exposing (..)
import Diagram.Render.Colour as Colour


draw : Config -> Line -> Svg.Svg msg
draw config model =
    case model.lineType of
        Full ->
            fullLine config model

        Dashed ->
            dashedLine config model

        LongDash ->
            longDashedLine config model


fullLine : Config -> Line -> Svg.Svg msg
fullLine config { coordinates, attributes, active } =
    let
        attrs =
            lineAttributes config attributes coordinates active
    in
        (getLineFn coordinates) attrs []


dashedLine : Config -> Line -> Svg.Svg msg
dashedLine config { coordinates, attributes, active } =
    let
        attrs =
            lineAttributes config attributes coordinates active
                |> dashIt
    in
        (getLineFn coordinates) attrs []


longDashedLine : Config -> Line -> Svg.Svg msg
longDashedLine config { coordinates, attributes, active } =
    let
        attrs =
            lineAttributes config attributes coordinates active
                |> longDashIt
    in
        (getLineFn coordinates) attrs []


getLineFn : LineCoordinates -> (List (Svg.Attribute msg) -> List (Svg.Svg msg) -> Svg.Svg msg)
getLineFn { between } =
    case between of
        Nothing ->
            Svg.line

        _ ->
            Svg.polyline


lineAttributes : Config -> List Attribute -> LineCoordinates -> Show -> List (Svg.Attribute msg)
lineAttributes config attrs ({ start, between, end } as coordinates) active =
    case between of
        Nothing ->
            lineAttrs start end
                |> (++) (lineStyle attrs active)

        _ ->
            let
                linePoints =
                    coordinatesToAttrs coordinates
            in
                linePoints :: (lineStyle attrs active)


lineStyle : List Attribute -> Show -> List (Svg.Attribute msg)
lineStyle attrs active =
    [ SvgA.strokeWidth "0.5"
    , SvgA.fill "none"
    , Colour.stroke active
    ]


lineAttrs : Point -> Point -> List (Svg.Attribute msg)
lineAttrs p1 p2 =
    let
        ( x1, y1 ) =
            coordsToString p1

        ( x2, y2 ) =
            coordsToString p2
    in
        [ SvgA.x1 x1
        , SvgA.y1 y1
        , SvgA.x2 x2
        , SvgA.y2 y2
        ]



{-
   lineAttrsDashed p1 p2 =
       lineAttrs p1 p2
           |> dashIt
-}


dashIt : List (Svg.Attribute msg) -> List (Svg.Attribute msg)
dashIt attrs =
    (SvgA.strokeDasharray "3, 1") :: attrs


longDashIt : List (Svg.Attribute msg) -> List (Svg.Attribute msg)
longDashIt attrs =
    (SvgA.strokeDasharray "2, 2") :: attrs


pointsToAttrs : List Point -> Svg.Attribute msg
pointsToAttrs pts =
    List.map pointToString pts
        |> String.join " "
        |> SvgA.points


coordinatesToAttrs : LineCoordinates -> Svg.Attribute msg
coordinatesToAttrs { start, between, end } =
    case between of
        Just ( p1, p2 ) ->
            pointsToAttrs [ start, p1, p2, end ]

        Nothing ->
            pointsToAttrs [ start, end ]
