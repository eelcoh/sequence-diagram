module Diagram.Compile.Session
    exposing
        ( toSession
        )

import Diagram.Lifeline as Lifeline
import Diagram.Compile.Types exposing (SessionPassTwo(..))
import Diagram.Types exposing (Config, Hash, Horizontal, Overlap(..), Identifier(..), Session, Sessions(..), Session(..), SessionDetails, Range(..), Show(..), Vertical, Y(..))
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
            toSessions detailsString sessionsPT

        sessionsString =
            Tuple.first stringsAndSessions

        sessionString =
            (detailsString ++ sessionsString)

        sessionId =
            Murmur3.hashString 24743 sessionString
    in
        Tuple.second stringsAndSessions
            |> Session sessionId attrs Hidden details ( incoming, outgoing )
            |> (,) sessionString


toSessions : String -> Sessions (List SessionPassTwo) -> ( String, Sessions (List Session) )
toSessions detailsString sessions =
    case sessions of
        Refer (Identifier i) ->
            let
                sessionsString =
                    String.join " / " [ "refer", "identifier", i, detailsString ]
            in
                ( sessionsString, Refer (Identifier i) )

        Sessions s ->
            listToSessions s


listToSessions : List SessionPassTwo -> ( String, Sessions (List Session) )
listToSessions sessions =
    List.map toSession sessions
        |> List.unzip
        |> (\( a, b ) -> ( (String.join " / " a), Sessions b ))


toSessionDetails : Horizontal -> Vertical -> SessionDetails
toSessionDetails { lifelineIdx, layerIdx } { start, end } =
    { start = Y.toInt start
    , end = Y.toInt end
    , lifelineIdx = lifelineIdx
    , layerIdx = X.toInt layerIdx
    }



-- end
