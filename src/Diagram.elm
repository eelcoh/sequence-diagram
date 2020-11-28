module Diagram exposing
    ( create
    , view, resize
    , Diagram, Errors
    )

{-| Create a sequence diagram in Elm.


# Initialise the diagram

@docs create


# Create the SVG

@docs view, resize


# Data

@docs Diagram, Errors

-}

import Diagram.Internal.DiagramData as DiagramData
import Diagram.Internal.Model as Model
import Diagram.Internal.Render.Config as Config
import Diagram.Internal.Render.Lifeline as Lifeline
import Diagram.Internal.Render.Session as RSession
import Diagram.Internal.Session as Session
import Diagram.Internal.Types as Types exposing (DiagramData, Identifier(..), Model, NamedSequences, Participant, Sequence, Session, Size)
import Dict
import List exposing (all)
import Result.Extra as ResultX
import Svg
import Svg.Attributes as SvgA


{-| The model of the diagram
-}
type alias Diagram =
    Model


{-| List of Strings
-}
type alias Errors =
    Types.Errors


{-| Initialise the diagram.
-}
create : List Participant -> Sequence -> List ( String, Sequence ) -> Result Errors Model
create participants sequence named =
    let
        namedSequences =
            Dict.fromList named

        rCurrent =
            DiagramData.create participants namedSequences sequence

        rTable =
            List.map (\( a, b ) -> ( a, DiagramData.create participants namedSequences b )) named
                -- > List (k, Result e v)
                |> List.map combine
                -- > List (Result e (k, v))
                |> ResultX.combine
                -- > Result e (List (k, v))
                |> Result.map Dict.fromList

        -- > Result e SessionTable
        createModel data table =
            Model.create data table Config.default
    in
    Result.map2 createModel rCurrent rTable


combine : ( String, Result Errors DiagramData ) -> Result Errors ( String, DiagramData )
combine x =
    let
        mapfn key rValue =
            Result.map (\b -> ( key, b )) rValue
    in
    (\( a, b ) -> mapfn a b) x


{-| Create the svg for the current state
-}
view : Model -> Svg.Svg msg
view model =
    let
        current =
            model.diagram

        lifelineLength =
            Session.max current.session

        lifelines ln =
            List.map (Lifeline.view model.config ln) current.lifelines

        participants =
            lifelines lifelineLength

        session =
            RSession.view model.config current.session

        elements =
            participants ++ [ session ]
    in
    Svg.svg
        --            [ version "1.1", x "0", y "0", viewBox "0 0 323.141 500.95" ]
        [ SvgA.version "1.1", SvgA.x "0", SvgA.y "0", SvgA.viewBox (dims model) ]
        elements


{-| Resize the diagram, convenient to use in case of window resize
-}
resize : Diagram -> Size -> Diagram
resize diagram size =
    let
        numParticipants =
            List.length diagram.diagram.lifelines

        sessionsHeight =
            Session.max diagram.diagram.session

        newConf =
            Config.size diagram.config size numParticipants sessionsHeight
    in
    { diagram | config = newConf }


dims : Model -> String
dims { config } =
    let
        w =
            config.size.width
                |> String.fromInt

        h =
            config.size.height
                |> String.fromInt
    in
    String.join " " [ "0", "0", w, h ]



--
