module Diagram.Navigate
    exposing
        ( first
        , next
        , prev
        , rewind
        , full
        , zoom
        , zoomOut
        )

{-| Navigate through a sequence diagram in Elm.

# Navigate the diagram
@docs first, next, prev, rewind, full, zoom, zoomOut

-}

import Diagram.Model as Model
import Diagram.Types as Types exposing (Model, Identifier(..))
import Diagram.Session as Session
import Dict


{-|
  Make the first session active, hide all others
-}
first : Model -> Model
first model =
    Model.move Session.first model


{-|
  Move one up.
-}
prev : Model -> Model
prev model =
    Model.move (Tuple.second << Session.prev) model


{-|
  Move back to the first
-}
rewind : Model -> Model
rewind =
    first


{-|
  Go to the next session.
-}
next : Model -> Model
next model =
    Model.move Session.next model


{-|
  Set the full diagram visible
-}
full : Model -> Model
full model =
    Model.move Session.full model


{-|
  Zoom into the referred sequence.
-}
zoom : Model -> Model
zoom model =
    let
        current =
            model.diagram

        mData =
            Session.getZoom current.session
                |> Maybe.map (\(Identifier i) -> i)
                |> Maybe.andThen (\i -> Dict.get i model.sessionTable)
    in
        case mData of
            Nothing ->
                model

            Just d ->
                let
                    newDiagram =
                        d

                    newStack =
                        model.diagram :: model.stack
                in
                    { model | diagram = newDiagram, stack = newStack }
                        |> full


{-|
  Zoom out of the referred sequence.
-}
zoomOut : Model -> Model
zoomOut model =
    case model.stack of
        a :: xs ->
            { model | diagram = a, stack = xs }

        _ ->
            model
