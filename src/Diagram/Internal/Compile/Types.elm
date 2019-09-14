module Diagram.Internal.Compile.Types exposing
    ( ArrowIn(..)
    , ArrowMeta
    , ArrowOut(..)
    , ArrowTempData
    , Arrows
    , IncomingArrowType(..)
    , SessionPassOne(..)
    , SessionPassTwo(..)
    )

import Diagram.Internal.Types exposing (..)


type SessionPassOne
    = SessionPassOne (List Attribute) LifelineIdx Vertical Arrows (Sessions (List SessionPassOne))


type SessionPassTwo
    = SessionPassTwo (List Attribute) Horizontal Vertical ( Maybe Arrow, Maybe Arrow ) (Sessions (List SessionPassTwo))


type alias ArrowMeta =
    { incoming : Maybe ArrowType
    , outgoing : Bool
    }


type alias Arrows =
    { incoming : Maybe ArrowTempData
    , outgoing : Maybe ArrowTempData
    }


type ArrowIn
    = Incoming Direction ArrowType


type ArrowOut
    = Outgoing Direction


type IncomingArrowType
    = SyncArrowType
    | AsyncArrowType


type alias ArrowTempData =
    { direction : Direction
    , arrowType : ArrowType
    , attributes : List Attribute
    }
