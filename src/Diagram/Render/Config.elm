module Diagram.Render.Config exposing (create, default, size, calculateBase)

import Diagram.Types exposing (Config, LifelineIdx(..), Size)


create : Size -> Config
create sz =
    { size = sz
    , width = 12.0
    , unitV = 12.0
    , unitH = 8.0
    , space = 64.0
    , layerOffset = 2.0
    }


default : Config
default =
    { size = (Size 400 600)
    , width = 10.0
    , unitV = 16.0
    , unitH = 10.0
    , space = 64.0
    , layerOffset = 4.0
    }


size : Config -> Size -> Config
size conf sz =
    { conf | size = sz }


calculateBase : Config -> LifelineIdx -> Float
calculateBase config (LifelineIdx lifelineIdx) =
    lifelineIdx
        |> toFloat
        |> (*) (config.space)
        |> (+) (config.space / 2)
