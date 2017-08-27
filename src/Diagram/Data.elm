module Diagram.Data exposing (..)

{-|
  Initialise the diagram.
-}

import Diagram.Participant as Participant
import Diagram.Compile exposing (compile)
import Diagram.Model as Model
import Diagram.Participant.Internal as Participant
import Diagram.Render.Config as Config
import Diagram.Render.Lifeline as Lifeline
import Diagram.Render.Session as RSession
import Diagram.Session as Session
import Diagram.Types as Types exposing (Data, Errors, Model, Participant, Sequence, Size, NamedSequences)
import Dict
import List exposing (all)
import Maybe.Extra
import Dict.Extra as DictX


create : List Participant -> NamedSequences -> Sequence -> Result Errors Data
create participants namedSequences sequence =
    let
        rCompiled =
            Participant.getIdentifiers namedSequences sequence
                |> Result.map (Participant.merge participants)
                |> Result.map (List.indexedMap (,))
                |> Result.andThen (\p -> compile p sequence namedSequences)

        createData ( lifelines, session ) =
            Data lifelines session
    in
        Result.map createData rCompiled
