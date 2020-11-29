module Diagram.Internal.Compile.NamedSequences exposing (error, get)

import Diagram.Internal.Types exposing (ActorType(..), Arrow(..), ArrowType(..), Attribute(..), Direction(..), Errors, Identifier(..), LineType(..), NamedSequences, Overlap(..), Participant(..), Point(..), Range(..), Sequence(..), Side(..))
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
