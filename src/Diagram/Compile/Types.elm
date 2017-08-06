module Diagram.Compile.Types
    exposing
        ( SessionPassOne(..)
        , SessionPassTwo(..)
        , ArrowMeta
        , Arrows
        , ArrowIn(..)
        , ArrowOut(..)
        , IncomingArrowType(..)
        , ArrowTempData
        )

import Diagram.Types exposing (..)


type SessionPassOne
    = SessionPassOne (List Attribute) LifelineIdx Vertical Arrows (List SessionPassOne)


type SessionPassTwo
    = SessionPassTwo (List Attribute) Horizontal Vertical ( Maybe Arrow, Maybe Arrow ) (List SessionPassTwo)


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
