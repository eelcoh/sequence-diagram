module Diagram
    exposing
        ( first
        , next
        , prev
        , rewind
        , full
        , view
        , create
        , resize
        , Diagram
        )

{-| Create a sequence diagram in Elm.


# Initialise the diagram
@docs create

# Navigate the diagram
@docs first, next, prev, rewind, full

# Create the SVG
@docs view, resize

# Data
@docs Diagram

-}

import Diagram.Compile exposing (compile)
import Diagram.Model as Model
import Diagram.Participant as Participant
import Diagram.Render.Config as Config
import Diagram.Render.Lifeline as Lifeline
import Diagram.Render.Session as RSession
import Diagram.Session as Session
import Diagram.Types exposing (Model, Participant, Sequence, Size)
import Dict
import List exposing (all)
import Maybe.Extra
import Svg
import Svg.Attributes as SvgA


{-| The model of the diagram
-}
type alias Diagram =
    Model


{-|
  Initialise the diagram.
-}
create : List Participant -> Sequence -> Model
create participants sequence =
    let
        {-
           namedSequences =
               Dict.fromList named

           namedSequenceParticipants =
               List.map Tuple.second named
                   |> List.map Participant.getIdentifiers
                   |> List.concat

           allParticipants =
               Participant.merge (Participant.getIdentifiers sequence) namedSequenceParticipants
                   |> Participant.merge participants
                   |> List.indexedMap (,)
        -}
        allParticipants =
            Participant.merge participants (Participant.getIdentifiers sequence)
                |> List.indexedMap (,)

        mCompiled =
            compile allParticipants sequence

        model =
            case mCompiled of
                Just ( lifelines, session ) ->
                    Model.create lifelines (Just session) (Config.default)

                Nothing ->
                    Model.empty (Config.default)
    in
        first model


{-|
  Make the first session active, hide all others
-}
first : Model -> Model
first model =
    let
        newSession =
            Maybe.map Session.first model.session
    in
        { model | session = newSession }


{-|
  Move one up.
-}
prev : Model -> Model
prev model =
    let
        newSession =
            Maybe.map Session.prev model.session
                |> Maybe.map Tuple.second
    in
        { model | session = newSession }


{-|
  Move back to the first
-}
rewind : Model -> Model
rewind =
    first


{-|
  Go to the next session.
-}
next : Model -> Model
next model =
    let
        newSession =
            Maybe.map Session.next model.session
                |> Maybe.map Tuple.second
    in
        { model | session = newSession }


{-|
  Set the full diagram visible
-}
full : Model -> Model
full model =
    let
        newSession =
            Maybe.map Session.full model.session
    in
        { model | session = newSession }


{-|
  Create the svg for the current state
-}
view : Model -> Svg.Svg msg
view model =
    let
        lifelineLength =
            Maybe.map Session.max model.session

        lifelines ln =
            List.map (Lifeline.view model.config ln) model.lifelines

        participants =
            Maybe.map lifelines lifelineLength

        session =
            Maybe.map (RSession.view model.config) model.session
                |> Maybe.Extra.toList

        elements =
            participants
                |> Maybe.map ((flip List.append) session)
                |> Maybe.withDefault []
    in
        Svg.svg
            --            [ version "1.1", x "0", y "0", viewBox "0 0 323.141 500.95" ]
            [ SvgA.version "1.1", SvgA.x "0", SvgA.y "0", SvgA.viewBox (dims model) ]
            elements


{-|
Resize the diagram, convenient to use in case of window resize
-}
resize : Diagram -> Size -> Diagram
resize diagram size =
    let
        newConf =
            Config.size diagram.config size
    in
        { diagram | config = newConf }


dims : Model -> String
dims { config } =
    let
        w =
            config.size.width
                |> toString

        h =
            config.size.height
                |> toString
    in
        String.join " " [ "0", "0", w, h ]



--
