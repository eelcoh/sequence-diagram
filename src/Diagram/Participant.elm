module Diagram.Participant exposing (..)

import Color
import Diagram.Attribute as Attributes
import Diagram.Result
import Diagram.Types exposing (..)
import Dict


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


getIdentifiers : NamedSequences -> Sequence -> Result Errors (List Identifier)
getIdentifiers namedSequences sequence =
    getIdentifierResults namedSequences sequence
        |> Diagram.Result.merge


getIdentifierResults : NamedSequences -> Sequence -> List (Result Errors Identifier)
getIdentifierResults namedSequences sequence =
    case sequence of
        Sequence identifier _ steps ->
            (Ok identifier) :: List.concat (List.map (getIdentifierResults namedSequences) steps)

        Synchronous identifier _ steps ->
            (Ok identifier) :: List.concat (List.map (getIdentifierResults namedSequences) steps)

        Asynchronous identifier _ steps ->
            (Ok identifier) :: List.concat (List.map (getIdentifierResults namedSequences) steps)

        RefSync (Identifier sequenceName) _ ->
            let
                mSeq =
                    Dict.get sequenceName namedSequences
            in
                case mSeq of
                    Nothing ->
                        let
                            error =
                                "Could not find sequence named "
                                    ++ sequenceName
                                    |> List.singleton
                        in
                            [ Err error ]

                    Just seq ->
                        getIdentifierResults namedSequences seq


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
