module Diagram.Compile exposing (compile)

import Diagram.Lifeline exposing (lifeline)
import Diagram.Compile.PassOne as PassOne
import Diagram.Compile.PassTwo as PassTwo
import Diagram.Compile.Session
import Diagram.Types as Compile exposing (Sequence, Participant, Lifeline, Session, Y(..), NamedSequences, Errors)


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
                    Diagram.Compile.Session.toSession s
            in
                ( l, session )
    in
        Result.map toSession mSessionPTandLifelines
