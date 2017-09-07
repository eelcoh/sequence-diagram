module Diagram.Internal.Compile exposing (compile)

import Diagram.Internal.Lifeline exposing (lifeline)
import Diagram.Internal.Compile.PassOne as PassOne
import Diagram.Internal.Compile.PassTwo as PassTwo
import Diagram.Internal.Compile.Session
import Diagram.Internal.Types as Compile exposing (Sequence, Participant, Lifeline, Session, Y(..), NamedSequences, Errors)


compile : List ( Int, Participant ) -> Sequence -> NamedSequences -> Result Errors ( List Lifeline, Session )
compile participants sequence namedSequences =
    let
        lifelines =
            List.map lifeline participants

        mSessionPTandLifelines =
            PassOne.pass (Y 3) Nothing lifelines sequence namedSequences
                |> Result.map (PassTwo.pass Nothing Nothing lifelines)

        toSession ( s, l ) =
            let
                ( _, session ) =
                    Diagram.Internal.Compile.Session.toSession s
            in
                ( l, session )
    in
        Result.map toSession mSessionPTandLifelines
