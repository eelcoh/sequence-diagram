module Diagram.Model exposing (..)

import Diagram.Types exposing (Data, SessionTable, Config, Lifeline, Model, Session)


{-
   empty : Config -> Model
   empty conf =
       Model [] Nothing conf
-}


create : Data -> SessionTable -> Config -> Model
create diagram sessionTable config =
    { diagram = diagram, sessionTable = sessionTable, stack = [], config = config }
