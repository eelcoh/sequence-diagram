module Diagram.Internal.Render.Rectangle exposing (toString)

import Diagram.Internal.Types exposing (Rectangle(..), RectangleAsString(..))


toString : Rectangle -> RectangleAsString
toString (Rectangle l t w h) =
    RectangleAsString
        (String.fromFloat l)
        (String.fromFloat t)
        (String.fromFloat w)
        (String.fromFloat h)
