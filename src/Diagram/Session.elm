module Diagram.Session
    exposing
        ( next
        , prev
        , hide
        , first
        , full
        , max
        )

import Diagram.Arrow as Arrow
import Diagram.Sessions as Sessions
import Diagram.Types exposing (Config, Hash, Horizontal, Overlap(..), Range(..), Session, Session(..), SessionDetails, Sessions(..), Show(..), Vertical, Y(..))


first : Session -> Session
first session =
    hide session
        |> activateThis


full : Session -> Session
full session =
    show session
        |> showThis


next : Session -> Session
next session =
    let
        finish ( c, s ) =
            let
                newSession =
                    case inspectSession session of
                        AllHidden ->
                            first s

                        AllVisible ->
                            hide s

                        Inspected ->
                            s

                -- reverse back
            in
                newSession
    in
        next_ session
            |> finish


next_ : Session -> ( Changed, Session )
next_ (Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    case active of
        Hidden ->
            (Session hash attributes Active sessionDetails ( mArrowIn, mArrowOut ) sessions)
                |> (,) Activated

        Active ->
            let
                ( activated, newSessions ) =
                    case sessions of
                        Sessions sess ->
                            nextSessions sess
                                |> \( a, s ) -> ( a, Sessions s )

                        Refer i ->
                            ( Deactivated, Refer i )
            in
                (Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) newSessions)
                    |> (,) activated

        Visible ->
            let
                ( activated, newSessions ) =
                    case sessions of
                        Sessions sess ->
                            nextSessions sess
                                |> \( a, s ) -> ( a, Sessions s )

                        Refer i ->
                            ( Unchanged, Refer i )
            in
                (Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) newSessions)
                    |> (,) activated


type Changed
    = Unchanged
    | Changed
    | Activated
    | Deactivated


nextSessions : List Session -> ( Changed, List Session )
nextSessions sessions =
    case sessions of
        [] ->
            ( Unchanged, sessions )

        x :: xs ->
            let
                ( activated, newSession ) =
                    next_ x
            in
                case activated of
                    Unchanged ->
                        let
                            ( activatedXs, newNextSessions ) =
                                nextSessions xs
                        in
                            newSession
                                :: newNextSessions
                                |> (,) activatedXs

                    Changed ->
                        newSession
                            :: xs
                            |> (,) Changed

                    Activated ->
                        case (getActive x) of
                            Active ->
                                newSession
                                    :: xs
                                    |> (,) Deactivated

                            _ ->
                                newSession
                                    :: xs
                                    |> (,) Changed

                    Deactivated ->
                        newSession
                            :: xs
                            |> (,) Changed



{- }

   if activated then
       newSession
           :: xs
           |> (,) activated
   else
       let
           ( activatedXs, newNextSessions ) =
               nextSessions xs
       in
           newSession
               :: newNextSessions
               |> (,) activated
-}


prev : Session -> ( Bool, Session )
prev session =
    let
        changedToBool changed =
            case changed of
                Unchanged ->
                    False

                _ ->
                    True

        finish ( c, s ) =
            let
                changed =
                    changedToBool c

                newSession =
                    case inspectSession s of
                        AllHidden ->
                            full s

                        AllVisible ->
                            activateLast s

                        Inspected ->
                            s

                -- reverse back
            in
                ( changed, newSession )
    in
        previous session
            |> finish


previous : Session -> ( Changed, Session )
previous session =
    case (getActive session) of
        Hidden ->
            session
                |> (,) Unchanged

        Active ->
            hideThis session
                |> (,) Deactivated

        Visible ->
            let
                sessions =
                    (Sessions.get session)

                ( changed, newSessions ) =
                    case sessions of
                        Refer i ->
                            ( Unchanged, sessions )

                        Sessions sessionList ->
                            prevSessions sessionList
                                |> \( a, b ) -> ( a, Sessions b )
            in
                case changed of
                    Deactivated ->
                        setSessions session newSessions
                            |> activateThis
                            |> (,) Changed

                    _ ->
                        setSessions session newSessions
                            |> (,) changed


prevSessions : List Session -> ( Changed, List Session )
prevSessions sessions =
    case sessions of
        [] ->
            ( Unchanged, sessions )

        x :: xs ->
            let
                ( changed, newSessions ) =
                    prevSessions xs

                s =
                    (Debug.log "s: " (List.length sessions))
            in
                case changed of
                    Unchanged ->
                        let
                            ( changedX, newSession ) =
                                previous x

                            -- db =
                            --   Debug.log "Unchanged" Unchanged
                        in
                            ( changedX, (newSession :: newSessions) )

                    Deactivated ->
                        let
                            newSession =
                                activateLast x

                            -- db =
                            --    Debug.log "Deactivated" Deactivated
                        in
                            ( Changed, (newSession :: newSessions) )

                    Changed ->
                        ( Changed, x :: newSessions )

                    Activated ->
                        ( Changed, x :: newSessions )


activateLast : Session -> Session
activateLast session =
    let
        ml =
            Sessions.get session

        h =
            Debug.log "activateLast " (getHash session)
    in
        case ml of
            Refer i ->
                activateThis session

            Sessions sessions ->
                case (activateLastOfSessions sessions) of
                    Nothing ->
                        activateThis session

                    Just newSessions ->
                        setSessions session (Sessions newSessions)


activateLastOfSessions : List Session -> Maybe (List Session)
activateLastOfSessions sessions =
    case sessions of
        [] ->
            Nothing

        s :: [] ->
            (activateLast s)
                :: []
                |> Just

        s :: xs ->
            (activateLastOfSessions xs)
                |> Maybe.map (\xss -> s :: xss)


activateThis : Session -> Session
activateThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        h =
            Debug.log "activateThis " hash
    in
        Session hash attributes Active sessionDetails ( mArrowIn, mArrowOut ) sessions


showThis : Session -> Session
showThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) sessions


hideThis : Session -> Session
hideThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        h =
            Debug.log "hideThis " hash
    in
        Session hash attributes Hidden sessionDetails ( mArrowIn, mArrowOut ) sessions


activate : Int -> Session -> ( Bool, Session )
activate hashToShow (Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    if hash == hashToShow then
        let
            newSession =
                Session hash attributes Active sessionDetails ( mArrowIn, mArrowOut ) sessions
        in
            ( True, newSession )
    else
        let
            any =
                List.any identity

            ( found, newSessions ) =
                case sessions of
                    Sessions sess ->
                        List.map (activate hashToShow) sess
                            |> List.unzip
                            |> \( a, b ) -> ( any a, Sessions b )

                    Refer i ->
                        ( False, Refer i )

            newStatus =
                if found then
                    Visible
                else
                    Hidden

            newSession =
                Session hash attributes newStatus sessionDetails ( mArrowIn, mArrowOut ) newSessions
        in
            ( found, newSession )


hide : Session -> Session
hide (Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        newSessions =
            Sessions.map hide sessions
    in
        (Session hash attributes Hidden sessionDetails ( mArrowIn, mArrowOut ) newSessions)


show : Session -> Session
show (Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        newSessions =
            Sessions.map full sessions
    in
        (Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) newSessions)


getHash : Session -> Hash
getHash (Session hash _ _ _ _ _) =
    hash


getActive : Session -> Show
getActive (Session _ _ show _ _ _) =
    show


setSessions : Session -> Sessions (List Session) -> Session
setSessions (Session hash attributes active sessionDetails arrows _) sessions =
    Session hash attributes active sessionDetails arrows sessions


max : Session -> Int
max (Session _ _ _ { end } ( _, arrowOut ) sessions) =
    let
        thisMax =
            Maybe.map Arrow.yEnd arrowOut
                |> Maybe.withDefault 0
                |> Basics.max end
    in
        case sessions of
            Sessions sessionList ->
                List.map max sessionList
                    |> (::) thisMax
                    |> List.maximum
                    |> Maybe.withDefault 0

            Refer _ ->
                1


mirror : Session -> Session
mirror session =
    let
        sessions =
            Sessions.get session
                |> Sessions.reverse
                |> Sessions.map mirror
    in
        setSessions session sessions


type Inspect
    = AllVisible
    | AllHidden
    | Inspected


inspectSession : Session -> Inspect
inspectSession session =
    let
        allstatuses =
            (getAllActive session)

        isVisible s =
            case s of
                Visible ->
                    True

                _ ->
                    False

        isHidden s =
            case s of
                Hidden ->
                    True

                _ ->
                    False

        isActive s =
            case s of
                Active ->
                    True

                _ ->
                    False

        allVisible =
            List.all isVisible allstatuses

        allHidden =
            List.all isHidden allstatuses
    in
        if allHidden then
            AllHidden
        else if allVisible then
            AllVisible
        else
            Inspected


getAllActives : Sessions (List Session) -> List Show
getAllActives sessions =
    case sessions of
        Refer i ->
            []

        Sessions sessionList ->
            List.map getAllActive sessionList
                |> List.concat


getAllActive : Session -> List Show
getAllActive session =
    (getActive session) :: (getAllActives (Sessions.get session))



-- end
