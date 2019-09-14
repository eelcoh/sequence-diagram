module Diagram.Internal.Compile.PassOne exposing (pass)

import Diagram.Internal.Compile.NamedSequences as Named
import Diagram.Internal.Compile.Sequence as Sequence
import Diagram.Internal.Compile.Types exposing (..)
import Diagram.Internal.Lifeline as Lifeline exposing (getDirection)
import Diagram.Internal.Types exposing (..)
import Diagram.Internal.Y as Y
import List.Extra as Extra



-- Pass one: get all vertical dimensions right


pass : Start -> Maybe LifelineIdx -> List Lifeline -> Sequence -> NamedSequences -> Result Errors SessionPassOne
pass start mLifelineIdxFrom lifelines sequence namedSequences =
    case sequence of
        Synchronous identifier attributes sequences ->
            let
                arrows =
                    { incoming = Just SyncArrow
                    , outgoing = True
                    }
            in
            addSession lifelines namedSequences mLifelineIdxFrom identifier attributes (Sessions sequences) start arrows

        Asynchronous identifier attributes sequences ->
            let
                arrows =
                    { incoming = Just AsyncArrow
                    , outgoing = False
                    }
            in
            addSession lifelines namedSequences mLifelineIdxFrom identifier attributes (Sessions sequences) start arrows

        Sequence identifier attributes sequences ->
            let
                arrows =
                    { incoming = Nothing
                    , outgoing = False
                    }
            in
            addSession lifelines namedSequences mLifelineIdxFrom identifier attributes (Sessions sequences) start arrows

        RefSync sequenceName attributes ->
            let
                arrows =
                    { incoming = Just SyncArrow
                    , outgoing = True
                    }

                doAddSession identifier =
                    addSession lifelines namedSequences mLifelineIdxFrom identifier attributes (Refer sequenceName) start arrows
            in
            Named.get sequenceName namedSequences
                |> Result.andThen (Sequence.getIdentifier namedSequences)
                |> Result.andThen doAddSession


addSession : List Lifeline -> NamedSequences -> Maybe LifelineIdx -> Identifier -> List Attribute -> Sessions (List Sequence) -> Start -> ArrowMeta -> Result Errors SessionPassOne
addSession lifelines namedSequences mLifelineIdxFrom identifier attrs sequences start arrowMeta =
    let
        withReturn =
            arrowMeta.outgoing

        mFirstSequence =
            case sequences of
                Sessions sequenceList ->
                    List.head sequenceList

                Refer _ ->
                    Nothing

        mLastSequence =
            case sequences of
                Sessions sequenceList ->
                    Extra.last sequenceList

                Refer _ ->
                    Nothing

        mLifelineIdx =
            Lifeline.findIndex identifier lifelines

        getLifelineIdx seq =
            Sequence.getLifelineIdx lifelines namedSequences seq
                |> Result.toMaybe

        mFirstSequenceLifelineIdxTo =
            mFirstSequence
                |> Maybe.andThen getLifelineIdx

        mLastSequenceLifelineIdxTo =
            mLastSequence
                |> Maybe.andThen getLifelineIdx

        -- Maybe.andThen (a -> M a) (a -> R a)
        mDirectionLast =
            Maybe.map2 getDirection mLifelineIdx mLastSequenceLifelineIdxTo

        mDirectionIn =
            Maybe.map2 getDirection mLifelineIdxFrom mLifelineIdx

        mDirectionFirst =
            Maybe.map2 getDirection mLifelineIdx mFirstSequenceLifelineIdxTo

        mDirectionBack =
            Maybe.map2 getDirection mLifelineIdx mLifelineIdxFrom

        addBefore =
            case mFirstSequence of
                Nothing ->
                    Y 0

                Just sequence ->
                    case Maybe.map2 (\a b -> ( a, b )) mDirectionIn mDirectionFirst of
                        Just ( ToSelf, ToSelf ) ->
                            Y 1

                        Just ( ToSelf, LeftToRight ) ->
                            Y 1

                        Just ( ToSelf, RightToLeft ) ->
                            Y 1

                        Just ( LeftToRight, ToSelf ) ->
                            Y 0

                        Just ( LeftToRight, LeftToRight ) ->
                            Y 0

                        Just ( LeftToRight, RightToLeft ) ->
                            Y 1

                        Just ( RightToLeft, ToSelf ) ->
                            Y 1

                        Just ( RightToLeft, LeftToRight ) ->
                            Y 1

                        Just ( RightToLeft, RightToLeft ) ->
                            Y 0

                        Nothing ->
                            Y 0

        addLast =
            case mLastSequence of
                Nothing ->
                    Y 0

                Just sequence ->
                    let
                        lastHasReturn =
                            case sequence of
                                Asynchronous _ _ _ ->
                                    True

                                Sequence _ _ _ ->
                                    True

                                Synchronous _ _ _ ->
                                    False

                                RefSync _ _ ->
                                    True
                    in
                    case Maybe.map2 (\c d -> ( withReturn, lastHasReturn, c, d )) mDirectionLast mDirectionBack of
                        Just ( False, _, _, _ ) ->
                            Y 0

                        Just ( True, False, ToSelf, _ ) ->
                            Y 1

                        Just ( True, False, _, _ ) ->
                            Y 0

                        Just ( _, _, ToSelf, ToSelf ) ->
                            Y 1

                        Just ( _, _, ToSelf, LeftToRight ) ->
                            Y 1

                        Just ( _, _, ToSelf, RightToLeft ) ->
                            Y 0

                        Just ( _, _, LeftToRight, ToSelf ) ->
                            Y 0

                        Just ( _, _, LeftToRight, LeftToRight ) ->
                            Y 0

                        Just ( _, _, LeftToRight, RightToLeft ) ->
                            Y 1

                        Just ( _, _, RightToLeft, ToSelf ) ->
                            Y 1

                        Just ( _, _, RightToLeft, LeftToRight ) ->
                            Y 0

                        Just ( _, _, RightToLeft, RightToLeft ) ->
                            Y 0

                        Nothing ->
                            Y 0

        arrowInExtra =
            case mDirectionIn of
                Just ToSelf ->
                    Y 1

                _ ->
                    Y 0

        arrowOutExtra =
            case Maybe.map (\b -> ( withReturn, b )) mDirectionBack of
                Just ( True, ToSelf ) ->
                    Y 1

                _ ->
                    Y 0

        arrowInStart =
            start

        sessionStart =
            Y.add arrowInStart arrowInExtra

        sessionFirstStart =
            Y.add sessionStart addBefore

        rNextSessions =
            case sequences of
                Sessions sequenceList ->
                    sessionsPassOne attrs sessionFirstStart mLifelineIdx lifelines namedSequences sequenceList
                        |> Result.map Sessions

                Refer identifier ->
                    Ok (Refer identifier)

        mLastSession =
            case rNextSessions of
                Ok (Sessions sessionList) ->
                    Extra.last sessionList

                _ ->
                    Nothing

        mLastArrowOut =
            Maybe.map (\(SessionPassOne _ _ vertical _ _) -> vertical.arrowOutEnd) mLastSession

        mLastEnd =
            Maybe.map (\(SessionPassOne _ _ vertical _ _) -> vertical.end) mLastSession

        sessionEnd =
            Maybe.withDefault (Y.add sessionStart (Y 1)) mLastEnd
                |> Y.add addLast

        arrowOutEnd =
            Y.add sessionEnd arrowOutExtra

        v =
            { start = sessionStart
            , end = sessionEnd
            , arrowInStart = arrowInStart
            , arrowOutEnd = arrowOutEnd
            }

        mkArrowIn direction hd =
            { attributes = attrs
            , direction = direction
            , arrowType = hd
            }

        arrowIn =
            Maybe.map2 mkArrowIn mDirectionIn arrowMeta.incoming

        arrowOut =
            if arrowMeta.outgoing then
                Maybe.map
                    (\d ->
                        { attributes = attrs
                        , direction = d
                        , arrowType = ReturnArrow
                        }
                    )
                    mDirectionBack

            else
                Nothing

        arrows =
            { incoming = arrowIn
            , outgoing = arrowOut
            }

        toPassOne mLifelineIdx rNxtSessions =
            case ( mLifelineIdx, rNxtSessions ) of
                ( _, Err error ) ->
                    Err error

                ( Nothing, Ok _ ) ->
                    Err [ "error" ]

                ( Just l, Ok ns ) ->
                    SessionPassOne attrs l v arrows ns
                        |> Ok
    in
    toPassOne mLifelineIdx rNextSessions


sessionsPassOne : List Attribute -> Start -> Maybe LifelineIdx -> List Lifeline -> NamedSequences -> List Sequence -> Result Errors (List SessionPassOne)
sessionsPassOne attrs start mLifelineIdxFrom lifelines namedSequences sequences =
    case sequences of
        [] ->
            Ok []

        sequence :: rest ->
            let
                first =
                    pass start mLifelineIdxFrom lifelines sequence namedSequences
            in
            case first of
                Err errors ->
                    Err errors

                Ok ((SessionPassOne _ _ v _ _) as session) ->
                    let
                        newStart =
                            Y.add v.arrowOutEnd (Y 1)
                    in
                    sessionsPassOne attrs newStart mLifelineIdxFrom lifelines namedSequences rest
                        |> Result.map ((::) session)
