module Diagram.Participant exposing (..)

import Diagram.Types exposing (..)
import Diagram.Attribute as Attributes
import Color


defaultParticipant : Identifier -> Participant
defaultParticipant identifier =
    Participant identifier (Attributes.system defaultParticipantStyle)


defaultParticipantStyle : List Attribute
defaultParticipantStyle =
    Attributes.empty
        |> Attributes.addBackgroundColour (Color.rgb 240 240 240)
        |> Attributes.addTextColour (Color.rgb 51 51 51)
        |> Attributes.addLineColour (Color.rgb 51 51 51)


participant : Identifier -> List Attribute -> Participant
participant identifier attributeList =
    Participant identifier attributeList



-- helpers


getIdentifiers : Sequence -> List Identifier
getIdentifiers sequence =
    case sequence of
        Sequence identifier _ steps ->
            identifier :: List.concat (List.map getIdentifiers steps)

        Synchronous identifier _ steps ->
            identifier :: List.concat (List.map getIdentifiers steps)

        Asynchronous identifier _ steps ->
            identifier :: List.concat (List.map getIdentifiers steps)


merge : List Participant -> List Identifier -> List Participant
merge participants sParticipants =
    List.foldr insert (List.reverse participants) sParticipants
        |> List.reverse


insert : Identifier -> List Participant -> List Participant
insert participantIdentifier participants =
    if (exists participants participantIdentifier) then
        participants
    else
        (defaultParticipant participantIdentifier) :: participants


exists : List Participant -> Identifier -> Bool
exists participants participantIdentifier =
    case participants of
        [] ->
            False

        (Participant identifier _) :: rest ->
            if (identifier == participantIdentifier) then
                True
            else
                exists rest participantIdentifier
