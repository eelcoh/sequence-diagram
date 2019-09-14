module Diagram.Internal.Tag exposing (hasTag, hasTags)


hasTags : List String -> List String -> Bool
hasTags tags tags2 =
    List.map (hasTag tags) tags2
        |> List.any identity


hasTag : List String -> String -> Bool
hasTag tags tag =
    List.member tag tags
