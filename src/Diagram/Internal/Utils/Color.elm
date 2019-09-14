module Diagram.Internal.Utils.Color exposing (colorToHex)

import Color exposing (Color)



-- Utils


colorToHex : Color -> String
colorToHex cl =
    let
        { red, green, blue } =
            Color.toRgba cl
    in
    List.map floor [ red, green, blue ]
        |> List.map toHex
        |> (::) "#"
        |> String.join ""


toHex : Int -> String
toHex =
    toRadix >> String.padLeft 2 '0'


toRadix : Int -> String
toRadix n =
    let
        getChr c =
            if c < 10 then
                String.fromInt c

            else
                String.fromChar <| Char.fromCode (87 + c)
    in
    if n < 16 then
        getChr n

    else
        toRadix (n // 16) ++ getChr (remainderBy 16 n)
