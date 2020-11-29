module Diagram.Navigate exposing (first, next, prev, rewind, full, zoom, zoomOut)

{-| Navigate through a sequence diagram in Elm.


# Navigate the diagram

@docs first, next, prev, rewind, full, zoom, zoomOut

-}

import Diagram.Internal.Model as Model
import Diagram.Internal.Session as Session
import Diagram.Internal.Types exposing (Identifier(..), Model)
import Dict


{-| Make the first session active, hide all others
-}
first : Model -> ( Maybe String, Model )
first model =
    Model.move Session.first model
        |> addActiveId


{-| Move one up.
-}
prev : Model -> ( Maybe String, Model )
prev model =
    Model.move (Tuple.second << Session.prev) model
        |> addActiveId


{-| Move back to the first
-}
rewind : Model -> ( Maybe String, Model )
rewind =
    first


{-| Go to the next session.
-}
next : Model -> ( Maybe String, Model )
next model =
    Model.move Session.next model
        |> addActiveId


{-| Set the full diagram visible
-}
full : Model -> ( Maybe String, Model )
full model =
    Model.move Session.full model
        |> addActiveId


{-| Zoom into the referred sequence.
-}
zoom : Model -> ( Maybe String, Model )
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
                |> addActiveId

        Just d ->
            let
                newDiagram =
                    d

                newStack =
                    model.diagram :: model.stack
            in
            { model | diagram = newDiagram, stack = newStack }
                |> full


{-| Zoom out of the referred sequence.
-}
zoomOut : Model -> ( Maybe String, Model )
zoomOut model =
    case model.stack of
        a :: xs ->
            { model | diagram = a, stack = xs }
                |> addActiveId

        _ ->
            model
                |> addActiveId


addActiveId : Model -> ( Maybe String, Model )
addActiveId model =
    Model.currentSession model
        |> Session.getCurrentActiveId
        |> (\b a -> (\c d -> ( c, d )) a b) model



-- end
