module Diagram.Internal.Session exposing
    ( first
    , full
    , getCurrentActiveId
    , getZoom
    , hide
    , highlight
    , max
    , next
    , prev
    )

import Diagram.Internal.Arrow as Arrow
import Diagram.Internal.Attribute as Attributes
import Diagram.Internal.Sessions as Sessions
import Diagram.Internal.Tag as Tag
import Diagram.Internal.Types exposing (Identifier(..), Overlap(..), Range(..), Session(..), Sessions(..), Show(..), Y(..))
import Maybe.Extra as MaybeX


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
        finish ( _, s ) =
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
            Session hash attributes Active sessionDetails ( mArrowIn, mArrowOut ) sessions
                |> (\b -> ( Activated, b ))

        Active ->
            let
                ( activated, newSessions ) =
                    case sessions of
                        Sessions sess ->
                            nextSessions sess
                                |> (\( a, s ) -> ( a, Sessions s ))

                        Refer i ->
                            ( Deactivated, Refer i )
            in
            Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) newSessions
                |> (\b -> ( activated, b ))

        Visible ->
            let
                ( activated, newSessions ) =
                    case sessions of
                        Sessions sess ->
                            nextSessions sess
                                |> (\( a, s ) -> ( a, Sessions s ))

                        Refer i ->
                            ( Unchanged, Refer i )
            in
            Session hash attributes active sessionDetails ( mArrowIn, mArrowOut ) newSessions
                |> (\b -> ( activated, b ))


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
                        |> (\b -> ( activatedXs, b ))

                Changed ->
                    newSession
                        :: xs
                        |> (\b -> ( Changed, b ))

                Activated ->
                    case getActive x of
                        Active ->
                            newSession
                                :: xs
                                |> (\b -> ( Deactivated, b ))

                        _ ->
                            newSession
                                :: xs
                                |> (\b -> ( Changed, b ))

                Deactivated ->
                    newSession
                        :: xs
                        |> (\b -> ( Changed, b ))



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
    case getActive session of
        Hidden ->
            session
                |> (\b -> ( Unchanged, b ))

        Active ->
            hideThis session
                |> (\b -> ( Deactivated, b ))

        Visible ->
            let
                sessions =
                    Sessions.get session

                ( changed, newSessions ) =
                    case sessions of
                        Refer _ ->
                            ( Unchanged, sessions )

                        Sessions sessionList ->
                            prevSessions sessionList
                                |> (\( a, b ) -> ( a, Sessions b ))
            in
            case changed of
                Deactivated ->
                    setSessions session newSessions
                        |> activateThis
                        |> (\b -> ( Changed, b ))

                _ ->
                    setSessions session newSessions
                        |> (\b -> ( changed, b ))


prevSessions : List Session -> ( Changed, List Session )
prevSessions sessions =
    case sessions of
        [] ->
            ( Unchanged, sessions )

        x :: xs ->
            let
                ( changed, newSessions ) =
                    prevSessions xs
            in
            case changed of
                Unchanged ->
                    let
                        ( changedX, newSession ) =
                            previous x
                    in
                    ( changedX, newSession :: newSessions )

                Deactivated ->
                    let
                        newSession =
                            activateLast x
                    in
                    ( Changed, newSession :: newSessions )

                Changed ->
                    ( Changed, x :: newSessions )

                Activated ->
                    ( Changed, x :: newSessions )


activateLast : Session -> Session
activateLast session =
    let
        ml =
            Sessions.get session
    in
    case ml of
        Refer _ ->
            activateThis session

        Sessions sessions ->
            case activateLastOfSessions sessions of
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
            activateLast s
                :: []
                |> Just

        s :: xs ->
            activateLastOfSessions xs
                |> Maybe.map (\xss -> s :: xss)


activateThis : Session -> Session
activateThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    Session hash attributes Active sessionDetails ( mArrowIn, mArrowOut ) sessions


showThis : Session -> Session
showThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) sessions


hideThis : Session -> Session
hideThis (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    Session hash attributes Hidden sessionDetails ( mArrowIn, mArrowOut ) sessions


hide : Session -> Session
hide (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        newSessions =
            Sessions.map hide sessions
    in
    Session hash attributes Hidden sessionDetails ( mArrowIn, mArrowOut ) newSessions


show : Session -> Session
show (Session hash attributes _ sessionDetails ( mArrowIn, mArrowOut ) sessions) =
    let
        newSessions =
            Sessions.map full sessions
    in
    Session hash attributes Visible sessionDetails ( mArrowIn, mArrowOut ) newSessions


getActive : Session -> Show
getActive (Session _ _ showw _ _ _) =
    showw


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


type Inspect
    = AllVisible
    | AllHidden
    | Inspected


inspectSession : Session -> Inspect
inspectSession session =
    let
        allstatuses =
            getAllActive session

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
        Refer _ ->
            []

        Sessions sessionList ->
            List.map getAllActive sessionList
                |> List.concat


getAllActive : Session -> List Show
getAllActive session =
    getActive session :: getAllActives (Sessions.get session)


getZoom : Session -> Maybe Identifier
getZoom session =
    case session of
        Session _ _ Active _ _ (Refer i) ->
            Just i

        Session _ _ Active _ _ (Sessions _) ->
            Nothing

        Session _ _ _ _ _ (Refer _) ->
            Nothing

        Session _ _ _ _ _ (Sessions sessions) ->
            List.map getZoom sessions
                |> List.foldr MaybeX.or Nothing


highlight : List String -> Session -> Session
highlight tags (Session hash attributes _ sessionDetails arrows sessions) =
    let
        newSessions =
            case sessions of
                Sessions ss ->
                    List.map (highlight tags) ss
                        |> Sessions

                Refer _ ->
                    sessions

        newActive =
            if Tag.hasTags (Attributes.getTags attributes) tags then
                Active

            else
                Visible
    in
    Session hash attributes newActive sessionDetails arrows newSessions


getCurrentActiveId : Session -> Maybe String
getCurrentActiveId (Session _ attributes active _ _ sessions) =
    case active of
        Active ->
            Attributes.getId attributes

        _ ->
            case sessions of
                Sessions ss ->
                    List.filterMap getCurrentActiveId ss
                        |> List.head

                Refer _ ->
                    Nothing



-- end
