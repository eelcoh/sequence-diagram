module Diagram.Compile.Session
    exposing
        ( toSession
        )

import Diagram.Lifeline as Lifeline
import Diagram.Compile.Types exposing (SessionPassTwo(..))
import Diagram.Types exposing (Config, Hash, Horizontal, Overlap(..), Session, Session(..), SessionDetails, Range(..), Show(..), Vertical, Y(..))
import Diagram.X as X
import Diagram.Y as Y
import Murmur3


toSession : SessionPassTwo -> ( String, Session )
toSession (SessionPassTwo attrs horizontal vertical ( incoming, outgoing ) sessionsPT) =
    let
        details =
            toSessionDetails horizontal vertical

        hToString { lifelineIdx, layerIdx } =
            String.concat
                [ Lifeline.toString lifelineIdx
                , X.toString layerIdx
                ]

        vToString { start, end } =
            String.concat
                [ Y.toString start
                , Y.toString end
                ]

        detailsString =
            String.concat
                [ hToString horizontal
                , vToString vertical
                ]

        stringsAndSessions =
            List.map toSession sessionsPT

        sessionsString =
            String.concat (List.map Tuple.first stringsAndSessions)

        sessionString =
            (detailsString ++ sessionsString)

        sessionId =
            Murmur3.hashString 24743 sessionString
    in
        List.map Tuple.second stringsAndSessions
            |> Session sessionId attrs Hidden details ( incoming, outgoing )
            |> (,) sessionString


toSessionDetails : Horizontal -> Vertical -> SessionDetails
toSessionDetails { lifelineIdx, layerIdx } { start, end } =
    { start = Y.toInt start
    , end = Y.toInt end
    , lifelineIdx = lifelineIdx
    , layerIdx = X.toInt layerIdx
    }



-- end
