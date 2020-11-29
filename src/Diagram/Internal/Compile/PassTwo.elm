module Diagram.Internal.Compile.PassTwo exposing (pass)

import Diagram.Internal.Compile.Arrow as Arrow
import Diagram.Internal.Compile.Types exposing (ArrowTempData, SessionPassOne(..), SessionPassTwo(..))
import Diagram.Internal.Layer exposing (registerSession)
import Diagram.Internal.Types exposing (Arrow, ArrowType(..), Attribute, Direction(..), Horizontal, Lifeline, Range(..), Sessions(..), Vertical, X(..))


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
                    ( Sessions [], lifelines )

        session =
            SessionPassTwo attrs horizontal vertical newArrows newSteps
    in
    ( session, newNwLifelines )


sessionsPassTwo : List Attribute -> Maybe Horizontal -> Maybe Vertical -> List Lifeline -> Sessions (List SessionPassOne) -> ( Sessions (List SessionPassTwo), List Lifeline )
sessionsPassTwo attributes mFromHorizontal mFromVertical lifelines steps =
    case steps of
        Refer i ->
            ( Refer i, lifelines )

        Sessions l ->
            let
                ( nwSessions, nwLifelines ) =
                    sessionsPassTwoList attributes mFromHorizontal mFromVertical lifelines l
            in
            ( Sessions nwSessions, nwLifelines )


sessionsPassTwoList : List Attribute -> Maybe Horizontal -> Maybe Vertical -> List Lifeline -> List SessionPassOne -> ( List SessionPassTwo, List Lifeline )
sessionsPassTwoList attributes mFromHorizontal mFromVertical lifelines steps =
    case steps of
        [] ->
            ( [], lifelines )

        session :: rest ->
            let
                ( nwSession, nwLifelines ) =
                    pass mFromHorizontal mFromVertical lifelines session

                ( nwSessions, nwNwLifelines ) =
                    sessionsPassTwoList attributes mFromHorizontal mFromVertical nwLifelines rest
            in
            ( nwSession :: nwSessions, nwNwLifelines )



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
        f h _ =
            Arrow.create attrs h vertical.arrowInStart horizontal vertical.start arrowTempData
    in
    Maybe.map2 f mHorizontalFrom mVerticalFrom


createOutgoingArrow : List Attribute -> Horizontal -> Vertical -> Maybe Horizontal -> Maybe Vertical -> ArrowTempData -> Maybe Arrow
createOutgoingArrow attrs horizontal vertical mHorizontalFrom mVerticalFrom arrowTempData =
    let
        f h _ =
            Arrow.create attrs horizontal vertical.end h vertical.arrowOutEnd arrowTempData
    in
    Maybe.map2 f mHorizontalFrom mVerticalFrom
