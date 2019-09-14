module Diagram.Attribute exposing
    ( backgroundColour, textColour, lineColour
    , addBackgroundColour, addTextColour, addLineColour
    , tag, id
    , caption, return
    , person, system
    , empty
    )

{-| Attributes for the sequence diagram styling and captions


# Styling buiders

@docs backgroundColour, textColour, lineColour


# Styling helpers

@docs addBackgroundColour, addTextColour, addLineColour


# Visibility Tags

@docs tag, id


# Caption builders

@docs caption, return


# participants

@docs person, system


# empty attribute set

@docs empty

-}

import Color exposing (Color)
import Diagram.Internal.Types exposing (..)


{-| -}
empty : List Attribute
empty =
    []


{-| -}
system : List Attribute -> List Attribute
system attributes =
    List.filter isActorAttribute attributes
        |> (::) (Actor System)


{-| -}
person : List Attribute -> List Attribute
person attributes =
    List.filter isActorAttribute attributes
        |> (::) (Actor Person)


isActorAttribute : Attribute -> Bool
isActorAttribute attribute =
    case attribute of
        Actor _ ->
            True

        BackgroundColour _ ->
            True

        TextColour _ ->
            True

        Caption _ ->
            True

        LineColour _ ->
            True

        _ ->
            False


{-| The caption provides a text. It is used
- for incoming arrows for sequence steps
- for participants, if the participant should use a different caption than
identifier (long identifiers can be cumbersome)
-}
caption : String -> Attribute
caption c =
    Caption c


{-| Return caption, used for sycnhronous calls
-}
return : String -> Attribute
return c =
    Return c


{-| Background color for the participants.
-}
backgroundColour : Color -> Attribute
backgroundColour c =
    BackgroundColour c


{-| Helper function for pipelining attributes:

empty
|> addBackgroundColour (Color.rgb 23 23 23)
|> addTextColour (Color.rgb 240 240 240)

-}
addBackgroundColour : Color -> List Attribute -> List Attribute
addBackgroundColour color attributes =
    BackgroundColour color
        |> (\a -> a :: attributes)


{-| Colour of texts (captions)
-}
textColour : Color -> Attribute
textColour c =
    TextColour c


{-| Helper function for pipelining attributes:

empty
|> addBackgroundColour (Color.rgb 23 23 23)
|> addTextColour (Color.rgb 240 240 240)

-}
addTextColour : Color -> List Attribute -> List Attribute
addTextColour color attributes =
    TextColour color
        |> (\a -> a :: attributes)


{-| Create an lineColour attribute, used for the line of the participants
and the line colour of the arrows
-}
lineColour : Color -> Attribute
lineColour c =
    LineColour c


{-| Helper function for pipelining attributes:

empty
|> addBackgroundColour (Color.rgb 23 23 23)
|> addTextColour (Color.rgb 240 240 240)
|> addLineColour (Color.rgb 51 51 51)

-}
addLineColour : Color -> List Attribute -> List Attribute
addLineColour color attributes =
    LineColour color
        |> (\a -> a :: attributes)


{-| Tag a session. Is used to highlight sessions.

  - Sessions can have zero or more Tags
  - Tags can be shared across sessions

-}
tag : String -> Attribute
tag t =
    Tag t


{-| Provide an id for a sessions, to be used to give context while navigating the diagram.
An would make sense to make them unique, but nothing breaks if they aren't.
-}
id : String -> Attribute
id i =
    Id i



--
