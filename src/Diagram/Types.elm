module Diagram.Types exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)


type alias Model =
    { stack : List Data
    , diagram : Data
    , sessionTable : SessionTable
    , config : Config
    }


type alias Data =
    { lifelines : List Lifeline
    , session : Session
    }


type Identifier
    = Identifier String


type Participant
    = Participant Identifier (List Attribute)


type Sequence
    = Synchronous Identifier (List Attribute) (List Sequence)
    | Asynchronous Identifier (List Attribute) (List Sequence)
    | Sequence Identifier (List Attribute) (List Sequence)
    | RefSync Identifier (List Attribute)


type alias NamedSequence =
    ( String, Sequence )


type alias NamedSequences =
    Dict.Dict String Sequence


type alias SessionTable =
    Dict.Dict String Data


type alias Errors =
    List String


{-| Attributes are
  - Colours for text, background and line.
  - Captions for the arrows and participants.
  - Actor for participants
  - PageRange for creating multiple images for presenting the sequence diagram step by step
-}
type Attribute
    = BackgroundColour Color
    | TextColour Color
    | LineColour Color
    | Caption String
    | Return String
    | Actor ActorType
    | None


type alias Config =
    { size : Size
    , width : Float
    , unitV : Float
    , unitH : Float
    , space : Float
    , layerOffset : Float
    }



{-
   type alias Diagram =
       { lifelines : List Lifeline
       , sessions : List Session
       }
-}
--module Sequence.Types exposing (..)
{- }

   type Diagram
       = Diagram (List Participant) Sequence
-}


type alias Lifeline =
    { participant : Participant
    , layers : List Layer
    , idx : LifelineIdx
    }


type Session
    = Session Hash (List Attribute) Show SessionDetails ( Maybe Arrow, Maybe Arrow ) (Sessions (List Session))


type Sessions a
    = Sessions a
    | Refer Identifier


type Show
    = Visible
    | Hidden
    | Active


type Arrow
    = Arrow (List Attribute) ArrowDetails


type alias Hash =
    Int


type alias SessionDetails =
    { start : Int
    , end : Int
    , lifelineIdx : LifelineIdx
    , layerIdx : Int
    }


type alias ArrowDetails =
    { start : Coordinate
    , end : Coordinate
    , arrowType : ArrowType
    , direction : Direction
    }



{-
   type alias ParticipantDetails =
       { lifeline : LifelineIdx
       , actor : ActorType
       }

-}


type alias LifelineDetails =
    { lifeline : LifelineIdx
    }


type alias Vertical =
    { start : Y
    , end : Y
    , arrowInStart : Y
    , arrowOutEnd : Y
    }


type alias Horizontal =
    { lifelineIdx : LifelineIdx
    , layerIdx : LayerIdx
    }


type ActorType
    = Person
    | System


type Head
    = Open
    | Closed


type ArrowType
    = SyncArrow
    | AsyncArrow
    | ReturnArrow


type Direction
    = LeftToRight
    | RightToLeft
    | ToSelf


type Layer
    = Layer (List Range)



-- Range


type Range
    = Range { start : Y, end : Y }


type Overlap
    = Before
    | Overlapping
    | After


type alias LayerIdx =
    X


type X
    = X Int


type alias Start =
    Y


type alias End =
    Y


type Y
    = Y Int


type LifelineIdx
    = LifelineIdx Int



-- Coordinates


type alias Coordinate =
    { lifelineIdx : LifelineIdx
    , layerIdx : LayerIdx
    , y : Y
    }


type Point
    = Point Float Float


type alias Rectangle =
    ( Float, Float, Float, Float )


type Side
    = RightSide
    | LeftSide
    | SelfUp
    | SelfDown
    | NoOffset


type alias LineCoordinates =
    { start : Point
    , between : Maybe ( Point, Point )
    , end : Point
    }


type LineType
    = Full
    | Dashed
    | LongDash


type alias Line =
    { coordinates : LineCoordinates
    , lineType : LineType
    , attributes : List Attribute
    , active : Show
    }


type alias Size =
    { width : Int
    , height : Int
    }



--
