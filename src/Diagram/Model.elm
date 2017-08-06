module Diagram.Model exposing (..)

import Diagram.Types exposing (Config, Lifeline, Model, Session)


empty : Config -> Model
empty conf =
    Model [] Nothing conf


create : List Lifeline -> Maybe Session -> Config -> Model
create lifelines session config =
    { lifelines = lifelines, session = session, config = config }
