module Diagram.Render.Session
    exposing
        ( view
        )

import Diagram.Attribute.Internal as Attributes
import Diagram.Render.Arrow as Arrow
import Diagram.Render.Colour as Colour
import Diagram.Render.Config as Config
import Diagram.Types exposing (Config, Hash, Horizontal, Overlap(..), Range(..), Session, Session(..), SessionDetails, Show(..), Vertical, Y(..))
import Svg exposing (Svg)
import Svg.Attributes as SvgA


-- view


view : Config -> Session -> Svg msg
view config session =
    let
        vThis =
            viewSession config session

        vSessions =
            List.map (view config) (sessions session)
    in
        Svg.g []
            (vThis ++ vSessions)


viewSession : Config -> Session -> List (Svg msg)
viewSession config (Session hash attributes active model ( mArrowIn, mArrowOut ) _) =
    let
        vArrowIn =
            Maybe.map (Arrow.view config active) mArrowIn

        vArrowOut =
            Maybe.map (Arrow.view config active) mArrowOut

        base =
            Config.calculateBase config model.lifelineIdx

        top =
            (toFloat model.start)
                * config.unitV
                |> toString

        height =
            ((toFloat model.end) - (toFloat model.start))
                * config.unitV
                |> toString

        left =
            base
                - (config.unitH * 0.5)
                + ((toFloat model.layerIdx) * config.layerOffset)
                |> toString

        width =
            config.unitH
                |> toString

        ( fill, line ) =
            case active of
                Active ->
                    ( "#FF6200", "#333" )

                Hidden ->
                    ( "#eee", "#aaa" )

                Visible ->
                    ( "#aaa", "#333" )

        attrs =
            [ SvgA.x left
            , SvgA.y top
            , SvgA.width width
            , SvgA.height height
            , SvgA.rx "0.1"
            , SvgA.ry "0.1"
            , Colour.border active
            , SvgA.strokeWidth "0.5"
            , Colour.fill active
            , SvgA.pointerEvents "all"
            , SvgA.cursor "pointer"
            ]
                |> Attributes.add attributes

        arrows =
            List.filterMap identity [ vArrowIn, vArrowOut ]
    in
        (Svg.rect attrs []) :: arrows


sessions : Session -> List Session
sessions (Session _ _ _ _ _ sessions) =
    sessions
