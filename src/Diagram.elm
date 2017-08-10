module Diagram
    exposing
        ( first
        , next
        , prev
        , rewind
        , full
        , zoom
        , zoomOut
        , view
        , create
        , resize
        , Diagram
        , Errors
        )

{-| Create a sequence diagram in Elm.


# Initialise the diagram
@docs create

# Navigate the diagram
@docs first, next, prev, rewind, full, zoom, zoomOut

# Create the SVG
@docs view, resize

# Data
@docs Diagram, Errors

-}

import Diagram.Compile exposing (compile)
import Diagram.Model as Model
import Diagram.Participant as Participant
import Diagram.Render.Config as Config
import Diagram.Render.Lifeline as Lifeline
import Diagram.Render.Session as RSession
import Diagram.Session as Session
import Diagram.Data as Data
import Diagram.Types as Types exposing (Data, Session, Model, Identifier(..), Participant, Sequence, Size, NamedSequences)
import Dict
import List exposing (all)
import Maybe.Extra
import Dict.Extra as DictX
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


{-|
  Initialise the diagram.
-}
create : List Participant -> Sequence -> List ( String, Sequence ) -> Result Errors Model
create participants sequence named =
    let
        namedSequences =
            Dict.fromList named

        rCurrent =
            Data.create participants namedSequences sequence

        rTable =
            List.map (\( a, b ) -> ( a, (Data.create participants namedSequences b) )) named
                -- > List (k, Result e v)
                |>
                    List.map combine
                -- > List (Result e (k, v))
                |>
                    ResultX.combine
                -- > Result e (List (k, v))
                |>
                    Result.map Dict.fromList

        -- > Result e SessionTable
        createModel data table =
            Model.create data table (Config.default)
    in
        Result.map2 createModel rCurrent rTable


combine : ( String, Result Errors Data ) -> Result Errors ( String, Data )
combine x =
    let
        mapfn key rValue =
            Result.map ((,) key) rValue
    in
        uncurry mapfn x


{-|
  Make the first session active, hide all others
-}
first : Model -> Model
first model =
    move Session.first model


{-|
  Move one up.
-}
prev : Model -> Model
prev model =
    move (Tuple.second << Session.prev) model


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
    move Session.next model


{-|
  Set the full diagram visible
-}
full : Model -> Model
full model =
    move Session.full model


{-|
  Zoom into the referred sequence.
-}
zoom : Model -> Model
zoom model =
    let
        current =
            model.diagram

        mData =
            Session.getZoom current.session
                |> Maybe.map (\(Identifier i) -> i)
                |> Maybe.andThen (\i -> Dict.get i model.sessionTable)
    in
        case mData of
            Nothing ->
                model

            Just d ->
                let
                    newDiagram =
                        d

                    newStack =
                        model.diagram :: model.stack
                in
                    { model | diagram = newDiagram, stack = newStack }
                        |> full


{-|
  Zoom out of the referred sequence.
-}
zoomOut : Model -> Model
zoomOut model =
    case model.stack of
        a :: xs ->
            { model | diagram = a, stack = xs }

        _ ->
            model



{- generic function
   takes a move function, and then creates a new model
-}


move : (Session -> Session) -> Model -> Model
move fn model =
    let
        current =
            model.diagram

        newSession =
            fn current.session

        newDiagram =
            { current | session = newSession }
    in
        { model | diagram = newDiagram }


{-|
  Create the svg for the current state
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
