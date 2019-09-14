module Flow exposing (FlowStack, current, init, next, prev)


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
next ({ prev, current, next } as flowStack) =
    case current of
        Nothing ->
            case pop next of
                Nothing ->
                    flowStack

                Just ( t, ts ) ->
                    FlowStack prev (Just t) ts

        Just c ->
            case pop next of
                Nothing ->
                    FlowStack (c :: prev) Nothing []

                Just ( t, ts ) ->
                    FlowStack (c :: prev) (Just t) ts


prev : FlowStack -> FlowStack
prev ({ prev, current, next } as flowStack) =
    case current of
        Nothing ->
            case pop prev of
                Nothing ->
                    flowStack

                Just ( t, ts ) ->
                    FlowStack ts (Just t) next

        Just c ->
            case pop prev of
                Nothing ->
                    FlowStack [] Nothing (c :: prev)

                Just ( t, ts ) ->
                    FlowStack ts (Just t) (c :: next)


pop : List a -> Maybe ( a, List a )
pop stack =
    case stack of
        [] ->
            Nothing

        x :: xs ->
            Just ( x, xs )
