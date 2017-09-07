module Diagram.Internal.DiagramData exposing (..)

{-|
  Initialise the diagram.
-}

import Diagram.Internal.Compile exposing (compile)
import Diagram.Internal.Participant as Participant
import Diagram.Internal.Types as Types exposing (DiagramData, Errors, Model, Participant, Sequence, Size, NamedSequences)
import List exposing (all)


create : List Participant -> NamedSequences -> Sequence -> Result Errors DiagramData
create participants namedSequences sequence =
    let
        rCompiled =
            Participant.getIdentifiers namedSequences sequence
                |> Result.map (Participant.merge participants)
                |> Result.map (List.indexedMap (,))
                |> Result.andThen (\p -> compile p sequence namedSequences)

        createData ( lifelines, session ) =
            DiagramData lifelines session
    in
        Result.map createData rCompiled
