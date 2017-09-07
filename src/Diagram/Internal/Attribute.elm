module Diagram.Internal.Attribute exposing (..)

import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Diagram.Internal.Types exposing (..)
import Svg
import Svg.Attributes exposing (color, cx, fill, stroke)


getCaption : List Attribute -> Maybe String
getCaption attributes =
    let
        isCaption a =
            case a of
                Caption s ->
                    Just s

                _ ->
                    Nothing
    in
        List.filterMap isCaption attributes
            |> List.head


getReturnCaption : List Attribute -> Maybe String
getReturnCaption attributes =
    let
        isReturnCaption a =
            case a of
                Return s ->
                    Just s

                _ ->
                    Nothing
    in
        List.filterMap isReturnCaption attributes
            |> List.head


getTextColour : List Attribute -> Maybe Color
getTextColour attributes =
    let
        isTextColour a =
            case a of
                TextColour c ->
                    Just c

                _ ->
                    Nothing
    in
        List.filterMap isTextColour attributes
            |> List.head


getBackgroundColour : List Attribute -> Maybe Color
getBackgroundColour attributes =
    let
        isBackgroundColour a =
            case a of
                BackgroundColour c ->
                    Just c

                _ ->
                    Nothing
    in
        List.filterMap isBackgroundColour attributes
            |> List.head


getLineColour : List Attribute -> Maybe Color
getLineColour attributes =
    let
        isLineColour a =
            case a of
                LineColour c ->
                    Just c

                _ ->
                    Nothing
    in
        List.filterMap isLineColour attributes
            |> List.head


getId : List Attribute -> Maybe String
getId attributes =
    let
        isId a =
            case a of
                Id i ->
                    Just i

                _ ->
                    Nothing
    in
        List.filterMap isId attributes
            |> List.head


none : Attribute
none =
    None



-- add


add : List Attribute -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
add newAttrs attrs =
    (List.filterMap toAttribute newAttrs) ++ attrs


addOne : Attribute -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
addOne attr attrs =
    case (toAttribute attr) of
        Just a ->
            a :: attrs

        Nothing ->
            attrs


toAttribute : Attribute -> Maybe (Svg.Attribute msg)
toAttribute attr =
    case attr of
        TextColour clr ->
            colorToHex clr
                |> Svg.Attributes.color
                |> Just

        LineColour clr ->
            colorToHex clr
                |> Svg.Attributes.stroke
                |> Just

        BackgroundColour clr ->
            colorToHex clr
                |> Svg.Attributes.fill
                |> Just

        _ ->
            Nothing


getTags : List Attribute -> List String
getTags attrs =
    let
        ifTag a =
            case a of
                Tag t ->
                    Just t

                _ ->
                    Nothing
    in
        List.filterMap ifTag attrs
