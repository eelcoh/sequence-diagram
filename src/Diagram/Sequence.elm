module Diagram.Sequence
    exposing
        ( getLifelineIdx
        )

import Diagram.Lifeline as Lifeline
import Diagram.Types exposing (..)


getLifelineIdx : List Lifeline -> Sequence -> Maybe LifelineIdx
getLifelineIdx lifelines sequence =
    getIdentifierForSequence sequence
        |> (flip Lifeline.findIndex) lifelines


getIdentifierForSequence : Sequence -> Identifier
getIdentifierForSequence sequence =
    case sequence of
        Synchronous identifier _ _ ->
            identifier

        Asynchronous identifier _ _ ->
            identifier

        Sequence identifier _ _ ->
            identifier



{-
   getIdentifierForSequence : NamedSequences -> Sequence -> Result Error Identifier
   getIdentifierForSequence namedSequences sequence  =
       case sequence of
           Synchronous identifier _ _ ->
               identifier

           Asynchronous identifier _ _ ->
               identifier

           Sequence identifier _ _ ->
               identifier

           Ref sequenceName _ ->
             Dict.get sequenceName namedSequences
             |> Maybe.map (getIdentifierForSequence namedSequences)
-}
-- end
