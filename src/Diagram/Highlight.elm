module Diagram.Highlight exposing (highlight, reset)

{-| Highlight parts of the sequence diagram in Elm.

# Highlight parts of the diagram
@docs highlight, reset

-}

import Diagram.Internal.Model as Model
import Diagram.Internal.Types as Types exposing (Model)
import Diagram.Internal.Session as Session


{-|
  Highlight all sessions with (one of the) given tags.
-}
highlight : List String -> Model -> Model
highlight tags model =
    Model.move (Session.highlight tags) model


{-|
  Un-highlight all sessions
-}
reset : Model -> Model
reset model =
    Model.move Session.full model
