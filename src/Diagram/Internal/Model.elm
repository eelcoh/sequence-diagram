module Diagram.Internal.Model exposing (..)

import Diagram.Internal.Types exposing (DiagramData, SessionTable, Config, Lifeline, Model, Session)


{-
   empty : Config -> Model
   empty conf =
       Model [] Nothing conf
-}


create : DiagramData -> SessionTable -> Config -> Model
create diagram sessionTable config =
    { diagram = diagram, sessionTable = sessionTable, stack = [], config = config }



{- generic function
   takes a move function, and then creates a new model
-}


move : (Session -> Session) -> Model -> Model
move fn model =
    let
        current =
            model.diagram

        newSession =
            fn current.session

        newDiagram =
            { current | session = newSession }
    in
        { model | diagram = newDiagram }


currentSession : Model -> Session
currentSession { diagram } =
    diagram.session
