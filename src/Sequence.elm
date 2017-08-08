module Sequence
    exposing
        ( sequence
        , sync
        , async
        , refSync
        , Sequence
        )

{-| Create a sequence diagram in Elm.

# Sequence buiders
@docs sequence, sync, async, refSync

# Data
@docs Sequence

-}

import Diagram.Types exposing (..)


{-| Sequences can be seen as trees of function calls
-}
type alias Sequence =
    Diagram.Types.Sequence


{-| Sequence: create a sequence without an incoming arrow. It is primarily
used for the first sequence. Any captions and returns in the attributes will be ignored.
-}
sequence : String -> List Attribute -> List Sequence -> Sequence
sequence participant attributes steps =
    Sequence (Identifier participant) attributes steps


{-| Sync: create a synchronous call to a lifeline. It is primarily
used for the first sequence. Will be initiated with a closed arrow.
-}
sync : String -> List Attribute -> List Sequence -> Sequence
sync participant attributes steps =
    Synchronous (Identifier participant) attributes steps


{-| Async: create an asynchronous call to a lifeline. It is primarily
used for the first sequence. Will be initiated with a open arrow and
there is no return arrow.
-}
async : String -> List Attribute -> List Sequence -> Sequence
async participant attributes steps =
    Asynchronous (Identifier participant) attributes steps


{-| refSync: create a synchronous call to referred sequence. This can be used to
reuse sequences.

The referred sequences will not be included directly, but will be used by zooming in.
-}
refSync : String -> List Attribute -> Sequence
refSync sequenceName attributes =
    RefSync (Identifier sequenceName) attributes


namedSequence : String -> Sequence -> NamedSequence
namedSequence sequenceName sequence =
    ( sequenceName, sequence )



--
