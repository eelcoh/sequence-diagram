module Diagram.Internal.Result exposing (errors, merge, oks)

import Diagram.Internal.Types exposing (Errors)


merge : List (Result Errors a) -> Result Errors (List a)
merge results =
    case errors results of
        [] ->
            oks results
                |> Ok

        xs ->
            Err xs


oks : List (Result Errors a) -> List a
oks results =
    case results of
        [] ->
            []

        (Ok a) :: res ->
            a :: oks res

        (Err _) :: res ->
            oks res


errors : List (Result Errors a) -> Errors
errors results =
    case results of
        [] ->
            []

        (Ok _) :: res ->
            errors res

        (Err e) :: res ->
            e ++ errors res
