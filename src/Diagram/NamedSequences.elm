module Diagram.NamedSequences exposing (..)

import Diagram.Types exposing (..)
import Dict


get : Identifier -> NamedSequences -> Result Errors Sequence
get (Identifier sequenceName) namedSequences =
    Dict.get sequenceName namedSequences
        |> Result.fromMaybe (error sequenceName)


error : String -> Errors
error sequenceName =
    "Could not resolve "
        ++ sequenceName
        |> List.singleton
