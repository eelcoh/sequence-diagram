module Diagram.Render.Config exposing (create, default, size, calculateBase)

import Diagram.Types exposing (Config, LifelineIdx(..), Size)


create : Size -> Config
create sz =
    { size = sz
    , width = 54.0
    , unitV = 12.0
    , unitH = 8.0
    , space = 10.0
    , layerOffset = 2.0
    }


default : Config
default =
    { size = (Size 400 600)
    , width = 54.0
    , unitV = 16.0
    , unitH = 10.0
    , space = 10.0
    , layerOffset = 4.0
    }


size : Config -> Size -> Int -> Int -> Config
size conf sz numParticipants sessionsHeight =
    let
        unitH =
            sz.width
                // ((5 * numParticipants) + (numParticipants - 1))
                |> toFloat

        space =
            unitH

        width =
            5 * unitH

        yFactor =
            (toFloat sz.height) / (toFloat sessionsHeight)

        unitV =
            1.6 * unitH

        layerOffset =
            0.4 * unitH
    in
        { conf | size = sz, unitH = unitH, width = width, space = space, unitV = unitV, layerOffset = layerOffset }


calculateBase : Config -> LifelineIdx -> Float
calculateBase config (LifelineIdx lifelineIdx) =
    lifelineIdx
        |> toFloat
        |> (*) (6 * config.unitH)
        |> (+) (config.width / 2)
