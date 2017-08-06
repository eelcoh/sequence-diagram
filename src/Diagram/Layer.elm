module Diagram.Layer
    exposing
        ( registerSession
        , toFloat
        )

import Diagram.Lifeline as Lifeline
import Diagram.Range as Range
import Diagram.Types exposing (Diagram, Layer(..), LayerIdx, Lifeline, LifelineIdx(..), Overlap(..), Session(..), Range, X(..))
import Diagram.X as X


empty : List Layer
empty =
    []


registerSession : Range -> LifelineIdx -> List Lifeline -> Maybe ( LayerIdx, List Lifeline )
registerSession session lifeline lifelines =
    Lifeline.lookUp lifelines lifeline
        |> Maybe.andThen (addSessionToLifeline session lifeline lifelines)


addSessionToLifeline : Range -> LifelineIdx -> List Lifeline -> Lifeline -> Maybe ( LayerIdx, List Lifeline )
addSessionToLifeline session lifelineIdx lifelines ({ layers } as lifeline) =
    addSessionToLayers session layers (X 0)
        |> updateComponent lifeline
        |> updateLifelines lifelines lifelineIdx


updateComponent : Lifeline -> ( LayerIdx, List Layer ) -> ( LayerIdx, Lifeline )
updateComponent lifeline ( layerIdx, layers ) =
    ( layerIdx, { lifeline | layers = layers } )


addSessionToLayers : Range -> List Layer -> LayerIdx -> ( LayerIdx, List Layer )
addSessionToLayers session layers ((X l) as layerIdx) =
    case layers of
        [] ->
            List.singleton session
                |> Layer
                |> List.singleton
                |> (,) layerIdx

        x :: xs ->
            case (addSessionToLayer session x) of
                Nothing ->
                    let
                        ( newLayerIdx, newLayers ) =
                            addSessionToLayers session xs (X (l + 1))
                    in
                        ( newLayerIdx, (x :: newLayers) )

                Just layer ->
                    ( layerIdx, List.singleton layer )


addSessionToLayer : Range -> Layer -> Maybe Layer
addSessionToLayer session (Layer sessions) =
    addSessionToLayerSessions session sessions
        |> Maybe.map Layer


addSessionToLayerSessions : Range -> List Range -> Maybe (List Range)
addSessionToLayerSessions session sessions =
    case sessions of
        [] ->
            Just (List.singleton session)

        x :: xs ->
            case (Range.overlap x session) of
                After ->
                    Just (session :: sessions)

                Before ->
                    addSessionToLayerSessions session xs
                        |> Maybe.map (\s -> x :: s)

                Overlapping ->
                    Nothing


updateLifelines : List Lifeline -> LifelineIdx -> ( LayerIdx, Lifeline ) -> Maybe ( LayerIdx, List Lifeline )
updateLifelines lifelines lifelineIdx ( layer, lifeline ) =
    let
        mNewLifelines =
            Lifeline.update lifelines lifelineIdx lifeline
    in
        Maybe.map ((,) layer) mNewLifelines


toFloat : LayerIdx -> Float
toFloat x =
    X.toFloat x



-- end
