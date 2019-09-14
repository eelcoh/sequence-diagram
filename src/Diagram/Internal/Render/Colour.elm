module Diagram.Internal.Render.Colour exposing (background, border, fill, head, line, none, stroke, text, txt)

import Color
import Color.Convert
import Diagram.Internal.Attribute as Attributes
import Diagram.Internal.Types exposing (Attribute, Show(..))
import Svg
import Svg.Attributes as SvgA


text : List Attribute -> Svg.Attribute msg
text attributes =
    Attributes.getTextColour attributes
        |> Maybe.withDefault (Color.rgb 51 51 51)
        |> Color.Convert.colorToHex
        |> SvgA.fill


line : List Attribute -> Svg.Attribute msg
line attributes =
    Attributes.getLineColour attributes
        |> Maybe.withDefault (Color.rgb 51 51 51)
        |> Color.Convert.colorToHex
        |> SvgA.stroke


background : List Attribute -> Svg.Attribute msg
background attributes =
    Attributes.getBackgroundColour attributes
        |> Maybe.withDefault (Color.rgb 51 51 51)
        |> Color.Convert.colorToHex
        |> SvgA.fill


stroke : Show -> Svg.Attribute msg
stroke active =
    case active of
        Hidden ->
            SvgA.stroke "none"

        Visible ->
            Color.rgb 51 51 51
                |> Color.Convert.colorToHex
                |> SvgA.stroke

        Active ->
            Color.rgb 255 98 0
                |> Color.Convert.colorToHex
                |> SvgA.stroke


border : Show -> Svg.Attribute msg
border active =
    case active of
        Hidden ->
            SvgA.stroke "none"

        Visible ->
            Color.rgb 51 51 51
                |> Color.Convert.colorToHex
                |> SvgA.stroke

        Active ->
            Color.rgb 214 99 27
                |> Color.Convert.colorToHex
                |> SvgA.stroke


head : Show -> Svg.Attribute msg
head active =
    case active of
        Hidden ->
            SvgA.fill "none"

        Visible ->
            Color.rgb 51 51 51
                |> Color.Convert.colorToHex
                |> SvgA.fill

        Active ->
            Color.rgb 255 98 0
                |> Color.Convert.colorToHex
                |> SvgA.fill


fill : Show -> Svg.Attribute msg
fill active =
    case active of
        Hidden ->
            SvgA.fill "none"

        Visible ->
            Color.rgb 200 200 200
                |> Color.Convert.colorToHex
                |> SvgA.fill

        Active ->
            Color.rgb 255 98 0
                |> Color.Convert.colorToHex
                |> SvgA.fill


txt : Show -> Svg.Attribute msg
txt active =
    case active of
        Hidden ->
            SvgA.fill "none"

        Visible ->
            Color.rgb 51 51 51
                |> Color.Convert.colorToHex
                |> SvgA.fill

        Active ->
            Color.rgb 255 98 0
                |> Color.Convert.colorToHex
                |> SvgA.fill


none : Svg.Attribute msg
none =
    SvgA.fill "none"
