module Diagram.Compile.PassOne
    exposing
        ( pass
        )

import Diagram.Lifeline as Lifeline exposing (getDirection)
import Diagram.Compile.Types exposing (..)
import Diagram.Types exposing (..)
import Diagram.Sequence as Sequence
import List.Extra as Extra
import Diagram.Y as Y


-- Pass one: get all vertical dimensions right


pass : Start -> Maybe LifelineIdx -> List Lifeline -> Sequence -> Maybe SessionPassOne
pass start mLifelineIdxFrom lifelines sequence =
    case sequence of
        Synchronous identifier attributes sequences ->
            let
                arrows =
                    { incoming = (Just SyncArrow)
                    , outgoing = True
                    }
            in
                addSession lifelines mLifelineIdxFrom identifier attributes sequences start arrows

        Asynchronous identifier attributes sequences ->
            let
                arrows =
                    { incoming = Just AsyncArrow
                    , outgoing = False
                    }
            in
                addSession lifelines mLifelineIdxFrom identifier attributes sequences start arrows

        Sequence identifier attributes sequences ->
            let
                arrows =
                    { incoming = Nothing
                    , outgoing = False
                    }
            in
                addSession lifelines mLifelineIdxFrom identifier attributes sequences start arrows


addSession : List Lifeline -> Maybe LifelineIdx -> Identifier -> List Attribute -> List Sequence -> Start -> ArrowMeta -> Maybe SessionPassOne
addSession lifelines mLifelineIdxFrom identifier attrs sequences start arrowMeta =
    let
        withReturn =
            arrowMeta.outgoing

        mFirstSequence =
            List.head sequences

        mLastSequence =
            Extra.last sequences

        mLifelineIdx =
            Lifeline.findIndex identifier lifelines

        mFirstSequenceLifelineIdxTo =
            mFirstSequence
                |> Maybe.andThen (Sequence.getLifelineIdx lifelines)

        mLastSequenceLifelineIdxTo =
            mLastSequence
                |> Maybe.andThen (Sequence.getLifelineIdx lifelines)

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
                    case Maybe.map2 (,) mDirectionIn mDirectionFirst of
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
                    in
                        case Maybe.map2 ((,,,) withReturn lastHasReturn) mDirectionLast mDirectionBack of
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
            case Maybe.map ((,) withReturn) mDirectionBack of
                Just ( True, ToSelf ) ->
                    Y 1

                _ ->
                    Y 0

        arrowInStart =
            Y.add start addBefore

        sessionStart =
            Y.add arrowInStart arrowInExtra

        nextSessions =
            sessionsPassOne attrs sessionStart mLifelineIdx lifelines sequences

        --sessionsPassOne attrs (Y.add start (Y.add addBefore arrowInExtra)) mLifelineIdx lifelines sequences
        mLastArrowOut =
            Maybe.map (\(SessionPassOne _ _ vertical _ _) -> vertical.arrowOutEnd) (Extra.last nextSessions)

        mLastEnd =
            Maybe.map (\(SessionPassOne _ _ vertical _ _) -> vertical.end) (Extra.last nextSessions)

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
    in
        Maybe.map (\l -> SessionPassOne attrs l v arrows nextSessions) mLifelineIdx


sessionsPassOne : List Attribute -> Start -> Maybe LifelineIdx -> List Lifeline -> List Sequence -> List SessionPassOne
sessionsPassOne attrs start mLifelineIdxFrom lifelines sequences =
    case sequences of
        [] ->
            []

        sequence :: rest ->
            let
                first =
                    pass start mLifelineIdxFrom lifelines sequence
            in
                case first of
                    Nothing ->
                        []

                    Just ((SessionPassOne _ _ v _ _) as session) ->
                        let
                            newStart =
                                Y.add v.arrowOutEnd (Y 1)
                        in
                            sessionsPassOne attrs newStart mLifelineIdxFrom lifelines rest
                                |> (::) session
