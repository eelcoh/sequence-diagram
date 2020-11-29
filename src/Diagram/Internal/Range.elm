module Diagram.Internal.Range exposing (getEnd, getStart, overlap)

import Diagram.Internal.Types exposing (Identifier(..), Overlap(..), Participant(..), Range(..), Sequence(..), Y)
import Diagram.Internal.Y as Y


getStart : Range -> Y
getStart (Range s) =
    s.start


getEnd : Range -> Y
getEnd (Range s) =
    s.end


{-| See if two pairs of ints have overlapping ranges
-}
overlap : Range -> Range -> Overlap
overlap (Range s1) (Range s2) =
    case Y.compare s1.start s2.end of
        GT ->
            After

        _ ->
            case Y.compare s1.end s2.start of
                LT ->
                    Before

                _ ->
                    Overlapping
