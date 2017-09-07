module Diagram.Internal.Compile.Arrow
    exposing
        ( create
        )

import Diagram.Internal.Compile.Coordinate exposing (..)
import Diagram.Internal.Compile.Types exposing (ArrowTempData)
import Diagram.Internal.Types exposing (..)


-- constructor functions
{-
   type alias Vertical =
       { start : Y
       , end : Y
       , arrowInStart : Y
       , arrowOutEnd : Y
       }
-}


create :
    List Attribute
    -> Horizontal
    -> Y
    -> Horizontal
    -> Y
    -> ArrowTempData
    -> Arrow
create attrs xStart yStart xEnd yEnd arrowTempData =
    let
        cStart =
            toCoordinate xStart.lifelineIdx xStart.layerIdx yStart

        --{ lifeline = xStart.lifeline
        --, y = yStart.start
        --, layer = xStart.layer
        --}
        cEnd =
            toCoordinate xEnd.lifelineIdx xEnd.layerIdx yEnd

        --{ lifeline = xEnd.lifeline
        --, layer = xEnd.layer
        --, y = yEnd.end
        --}
        details =
            { start = cStart
            , end = cEnd
            , arrowType = arrowTempData.arrowType
            , direction = arrowTempData.direction
            }
    in
        Arrow arrowTempData.attributes details



-- end
