module Diagram.Internal.Render.Lifeline exposing (view)

import Diagram.Internal.Attribute as Attributes
import Diagram.Internal.Render.Colour as Colour
import Diagram.Internal.Render.Config exposing (calculateBase)
import Diagram.Internal.Render.Line
import Diagram.Internal.Render.Point as Point
import Diagram.Internal.Types as Diagram exposing (..)
import Maybe exposing (withDefault)
import Svg exposing (..)
import Svg.Attributes as SvgA exposing (..)


view : Config -> Int -> Lifeline -> Svg msg
view config ln { participant, idx } =
    let
        ( l, t, w, h ) =
            getCoordinatesForComponent config idx

        ( mt, tt, wt, ht ) =
            ( l + (w / 2.0), t + (h / 2.0), w, h )
                |> Point.stringify

        ( lc, tc, wc, hc ) =
            ( l, t, w, h )
                |> Point.stringify

        yCenter =
            t + (h / 2.0)

        pt ( x, y ) =
            String.join "," [ toString x, toString y ]

        pts cs =
            List.map pt cs
                |> String.join " "

        caption =
            getAttributes participant
                |> Attributes.getCaption
                |> Maybe.withDefault (getIdentifier participant)
                |> vCaption

        txtColor =
            getAttributes participant
                |> Colour.text

        lineColor =
            getAttributes participant
                |> Colour.line

        backgroundColour =
            getAttributes participant
                |> Colour.background

        vCaption c =
            let
                attrs =
                    [ SvgA.x mt
                    , SvgA.y tt
                    , txtColor
                    , SvgA.fontFamily "Monospace"
                    , SvgA.fontSize "8"
                    , SvgA.dominantBaseline "middle"
                    , SvgA.cursor "pointer"
                    ]
            in
            Svg.text_
                attrs
                [ Svg.text c ]

        thisComponent =
            let
                attrs =
                    [ SvgA.x lc
                    , SvgA.y tc
                    , SvgA.width wc
                    , SvgA.height hc
                    , SvgA.strokeWidth "0.2"
                    , backgroundColour

                    --  , SvgA.pointerEvents "all"
                    ]
            in
            Svg.rect
                attrs
                []

        ll =
            viewLifeline config (getAttributes participant) idx ln

        elements =
            --[ (Just thisComponent)
            [ thisComponent
            , caption
            , ll
            ]
    in
    Svg.g [ SvgA.textAnchor "middle" ]
        elements


getCoordinatesForComponent : Config -> LifelineIdx -> Rectangle
getCoordinatesForComponent config lifelineIdx =
    let
        lifelineBase =
            calculateBase config lifelineIdx

        width =
            config.width

        stepHeight =
            config.unitV

        x =
            lifelineBase - (width / 2)

        y =
            0

        height =
            stepHeight * 1.6
    in
    ( x, y, width, height )


getCoordinatesForLifeline : Config -> LifelineIdx -> Int -> ( Point, Point )
getCoordinatesForLifeline config lifelineIdx ln =
    let
        x =
            calculateBase config lifelineIdx

        stepHeight =
            config.unitV

        y1 =
            stepHeight * 1.6

        y2 =
            y1 + (stepHeight * Basics.toFloat ln)
    in
    ( Point x y1, Point x y2 )


viewLifeline : Config -> List Diagram.Attribute -> LifelineIdx -> Int -> Svg msg
viewLifeline config attrs lifelineIdx lifelineLength =
    let
        ( top, bottom ) =
            getCoordinatesForLifeline config lifelineIdx lifelineLength

        lc =
            { start = top
            , between = Nothing
            , end = bottom
            }

        l =
            Line lc LongDash attrs Visible
    in
    Diagram.Internal.Render.Line.draw config l


getAttributes : Participant -> List Diagram.Attribute
getAttributes (Participant _ attrs) =
    attrs


getIdentifier : Participant -> String
getIdentifier (Participant (Identifier s) _) =
    s



--
