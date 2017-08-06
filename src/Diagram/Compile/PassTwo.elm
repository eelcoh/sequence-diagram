module Diagram.Compile.PassTwo
    exposing
        ( pass
        )

import Diagram.Compile.Arrow as Arrow
import Diagram.Layer exposing (registerSession)
import Diagram.Compile.Types exposing (..)
import Diagram.Types exposing (Arrow, Attribute, ArrowType(..), Coordinate, Diagram, Direction(..), Range(..), Horizontal, LayerIdx, Lifeline, LifelineIdx, Vertical, X(..), Y)


pass : Maybe Horizontal -> Maybe Vertical -> List Lifeline -> SessionPassOne -> ( SessionPassTwo, List Lifeline )
pass mHorizontalFrom mVerticalFrom lifelines (SessionPassOne attrs lifeline vertical arrows steps) =
    let
        mkSession { start, end } =
            Range { start = start, end = end }

        sessionY =
            mkSession vertical

        mLayerAndDiagram =
            registerSession sessionY lifeline lifelines

        horizontal =
            case mLayerAndDiagram of
                Just ( layer, _ ) ->
                    { lifelineIdx = lifeline, layerIdx = layer }

                Nothing ->
                    { lifelineIdx = lifeline, layerIdx = X 0 }

        -- Horizontal -> Vertical -> Horizontal -> Vertical -> ArrowIn
        mNewArrowIn =
            Maybe.map (createIncomingArrow attrs mHorizontalFrom mVerticalFrom horizontal vertical) arrows.incoming
                |> Maybe.withDefault Nothing

        mNewArrowOut =
            Maybe.map (createOutgoingArrow attrs horizontal vertical mHorizontalFrom mVerticalFrom) arrows.outgoing
                |> Maybe.withDefault Nothing

        newArrows =
            ( mNewArrowIn
            , mNewArrowOut
            )

        ( newSteps, newNwLifelines ) =
            case mLayerAndDiagram of
                Just ( _, newLifelines ) ->
                    sessionsPassTwo attrs (Just horizontal) (Just vertical) newLifelines steps

                Nothing ->
                    ( [], lifelines )

        session =
            SessionPassTwo attrs horizontal vertical newArrows newSteps
    in
        ( session, newNwLifelines )


sessionsPassTwo : List Attribute -> Maybe Horizontal -> Maybe Vertical -> List Lifeline -> List SessionPassOne -> ( List SessionPassTwo, List Lifeline )
sessionsPassTwo attributes mFromHorizontal mFromVertical lifelines steps =
    case steps of
        [] ->
            ( [], lifelines )

        session :: rest ->
            let
                ( nwSession, nwLifelines ) =
                    pass mFromHorizontal mFromVertical lifelines session

                ( nwSessions, nwNwLifelines ) =
                    sessionsPassTwo attributes mFromHorizontal mFromVertical nwLifelines rest
            in
                ( (nwSession :: nwSessions), nwNwLifelines )



{-
   type alias Vertical =
       { start : Y
       , end : Y
       , arrowInStart : Y
       , arrowOutEnd : Y
       }
-}


createIncomingArrow : List Attribute -> Maybe Horizontal -> Maybe Vertical -> Horizontal -> Vertical -> ArrowTempData -> Maybe Arrow
createIncomingArrow attrs mHorizontalFrom mVerticalFrom horizontal vertical arrowTempData =
    let
        f h v =
            Arrow.create attrs h vertical.arrowInStart horizontal vertical.start arrowTempData
    in
        Maybe.map2 f mHorizontalFrom mVerticalFrom


createOutgoingArrow : List Attribute -> Horizontal -> Vertical -> Maybe Horizontal -> Maybe Vertical -> ArrowTempData -> Maybe Arrow
createOutgoingArrow attrs horizontal vertical mHorizontalFrom mVerticalFrom arrowTempData =
    let
        f h v =
            Arrow.create attrs horizontal vertical.end h vertical.arrowOutEnd arrowTempData
    in
        Maybe.map2 f mHorizontalFrom mVerticalFrom
