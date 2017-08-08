module Diagram.Sessions exposing (..)

import Diagram.Types exposing (Sessions(..), Session(..))


map : (a -> b) -> Sessions (List a) -> Sessions (List b)
map f sessions =
    case sessions of
        Refer i ->
            Refer i

        Sessions s ->
            List.map f s
                |> Sessions


reverse : Sessions (List a) -> Sessions (List a)
reverse sessions =
    case sessions of
        Refer i ->
            Refer i

        Sessions s ->
            List.reverse s
                |> Sessions


get : Session -> Sessions (List Session)
get (Session _ _ _ _ _ sessions) =
    sessions
