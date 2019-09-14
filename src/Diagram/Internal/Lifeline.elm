module Diagram.Internal.Lifeline exposing
    ( findIndex
    , getDirection
    , lifeline
    , lookUp
    , toFloat
    , toInt
    , toString
    , update
    )

import Diagram.Internal.Types exposing (Direction(..), Identifier(..), Lifeline, LifelineIdx(..), Participant(..), X(..))
import List.Extra as Extra exposing (getAt, setAt)


lifeline : ( Int, Participant ) -> Lifeline
lifeline ( idx, participant ) =
    { participant = participant
    , layers = []
    , idx = LifelineIdx idx
    }


findIndex : Identifier -> List Lifeline -> Maybe LifelineIdx
findIndex identifier lifelines =
    Extra.find (isParticipant identifier) lifelines
        |> Maybe.map .idx


lookUp : List Lifeline -> LifelineIdx -> Maybe Lifeline
lookUp lifelines (LifelineIdx l) =
    getAt l lifelines


isParticipant : Identifier -> Lifeline -> Bool
isParticipant identifier { participant, layers } =
    getParticipantId participant
        |> equalIdentifier identifier


equalIdentifier : Identifier -> Identifier -> Bool
equalIdentifier (Identifier id1) (Identifier id2) =
    id1 == id2


equalLifelineIdx : LifelineIdx -> LifelineIdx -> Bool
equalLifelineIdx (LifelineIdx l1) (LifelineIdx l2) =
    l1 == l2


getParticipantId : Participant -> Identifier
getParticipantId (Participant cId _) =
    cId


update : List Lifeline -> LifelineIdx -> Lifeline -> Maybe (List Lifeline)
update lifelines (LifelineIdx l) lifeline_ =
    setAt l lifeline_ lifelines
        |> Just


getDirection : LifelineIdx -> LifelineIdx -> Direction
getDirection (LifelineIdx l1) (LifelineIdx l2) =
    case compare l1 l2 of
        GT ->
            RightToLeft

        EQ ->
            ToSelf

        LT ->
            LeftToRight


toInt : LifelineIdx -> Int
toInt (LifelineIdx x) =
    x


toFloat : LifelineIdx -> Float
toFloat (LifelineIdx x) =
    Basics.toFloat x


toString : LifelineIdx -> String
toString (LifelineIdx x) =
    "L" ++ String.fromInt x



--
