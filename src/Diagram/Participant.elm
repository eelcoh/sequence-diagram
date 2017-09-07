module Diagram.Participant
    exposing
        ( person
        , system
        , Participant
        )

{-| Create a sequence diagram in Elm.

# Participant builders
@docs person, system

# Data
@docs Participant

-}

import Diagram.Attribute as Attribute
import Diagram.Internal.Participant as Participant
import Diagram.Internal.Types exposing (..)


{-| Participants
-}
type alias Participant =
    Diagram.Internal.Types.Participant


{-| Person: a function to declare a human participant in the lifeline
-}
person : String -> List Attribute -> Participant
person identifier attributes =
    Participant.participant (Identifier identifier) (Attribute.person attributes)


{-| system: a function to declare an automated participant in the lifeline
-}
system : String -> List Attribute -> Participant
system identifier attributes =
    Participant (Identifier identifier) (Attribute.system attributes)



--
