module Diagram.Compile.Sequence
    exposing
        ( getLifelineIdx
        , getIdentifier
        )

import Diagram.Lifeline as Lifeline
import Diagram.Types exposing (..)
import Diagram.NamedSequences as Named


getLifelineIdx : List Lifeline -> NamedSequences -> Sequence -> Result Errors LifelineIdx
getLifelineIdx lifelines namedSequences sequence =
    let
        findIndex identifier =
            Lifeline.findIndex identifier lifelines
                |> Result.fromMaybe (err identifier)

        err (Identifier i) =
            "Could not resolve participant "
                ++ i
                |> List.singleton
    in
        getIdentifier namedSequences sequence
            |> Result.andThen findIndex


getIdentifier : NamedSequences -> Sequence -> Result Errors Identifier
getIdentifier namedSequences sequence =
    case sequence of
        Synchronous identifier _ _ ->
            Ok identifier

        Asynchronous identifier _ _ ->
            Ok identifier

        Sequence identifier _ _ ->
            Ok identifier

        RefSync sequenceName _ ->
            Named.get sequenceName namedSequences
                |> Result.andThen (getIdentifier namedSequences)



{-
   getIdentifier : NamedSequences -> Sequence -> Result Error Identifier
   getIdentifier namedSequences sequence  =
       case sequence of
           Synchronous identifier _ _ ->
               identifier

           Asynchronous identifier _ _ ->
               identifier

           Sequence identifier _ _ ->
               identifier

           Ref sequenceName _ ->
             Dict.get sequenceName namedSequences
             |> Maybe.map (getIdentifier namedSequences)
-}
-- end
