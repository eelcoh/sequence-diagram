module Highlight.Flow exposing (FlowStack, current, init, next, prev)


type alias FlowStack =
    { prev : List String
    , current : Maybe String
    , next : List String
    }


init : List String -> FlowStack
init stack =
    { prev = []
    , current = Nothing
    , next = stack
    }


current : FlowStack -> Maybe String
current flowStack =
    flowStack.current


next : FlowStack -> FlowStack
next flowStack =
    case flowStack.current of
        Nothing ->
            case pop flowStack.next of
                Nothing ->
                    flowStack

                Just ( t, ts ) ->
                    FlowStack flowStack.prev (Just t) ts

        Just c ->
            case pop flowStack.next of
                Nothing ->
                    FlowStack (c :: flowStack.prev) Nothing []

                Just ( t, ts ) ->
                    FlowStack (c :: flowStack.prev) (Just t) ts


prev : FlowStack -> FlowStack
prev flowStack =
    case flowStack.current of
        Nothing ->
            case pop flowStack.prev of
                Nothing ->
                    flowStack

                Just ( t, ts ) ->
                    FlowStack ts (Just t) flowStack.next

        Just c ->
            case pop flowStack.prev of
                Nothing ->
                    FlowStack [] Nothing (c :: flowStack.prev)

                Just ( t, ts ) ->
                    FlowStack ts (Just t) (c :: flowStack.next)


pop : List a -> Maybe ( a, List a )
pop stack =
    case stack of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )
