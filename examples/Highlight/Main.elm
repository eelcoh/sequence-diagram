module Main exposing (Model, Msg(..), app, containerStyle, diagramStyle, explanationStyle, init, initFlow, keyCodes, keyPressDispatcher, main, subscriptions, texts, update, view, viewDiagram, viewError, viewExplanation)

import Color
import Diagram exposing (Diagram, Errors)
import Diagram.Highlight exposing (highlight, reset)
import Dict
import Flow exposing (FlowStack)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Keyboard
import Navigation
import Sequence
import Window


type alias Model =
    { diagram : Result Errors Diagram
    , flow : FlowStack
    }


type Msg
    = Start
    | Next
    | Previous
    | NewLocation Navigation.Location
    | WindowResizes Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Start ->
            let
                diagram =
                    Result.map reset model.diagram

                flow =
                    Flow.init initFlow
            in
            ( { model | diagram = diagram, flow = flow }, Cmd.none )

        Next ->
            let
                newFlow =
                    Flow.next model.flow

                diagram =
                    case Flow.current newFlow of
                        Just t ->
                            Result.map (highlight [ t ]) model.diagram

                        Nothing ->
                            Result.map reset model.diagram
            in
            ( { model | diagram = diagram, flow = newFlow }, Cmd.none )

        Previous ->
            let
                newFlow =
                    Flow.prev model.flow

                diagram =
                    case Flow.current newFlow of
                        Just t ->
                            Result.map (highlight [ t ]) model.diagram

                        Nothing ->
                            Result.map reset model.diagram
            in
            ( { model | diagram = diagram, flow = newFlow }, Cmd.none )

        NewLocation l ->
            ( model, Cmd.none )

        WindowResizes windowSize ->
            let
                diagramWidth =
                    (windowSize.width // 5) * 3

                wSize =
                    Window.Size diagramWidth windowSize.height

                diagram =
                    Result.map (\d -> Diagram.resize d wSize) model.diagram
            in
            ( { model | diagram = diagram }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


keyCodes : Dict.Dict Int Msg
keyCodes =
    Dict.fromList
        [ ( 36, Start )

        -- home
        , ( 32, Next )

        -- space
        , ( 39, Next )

        -- arrow right
        , ( 82, Start )

        -- r for rewind
        , ( 83, Start )

        -- s for start
        , ( 68, Next )

        -- d
        , ( 78, Next )

        -- n
        , ( 37, Previous )

        -- arrow left
        , ( 80, Previous )

        -- p
        ]


keyPressDispatcher : Int -> Msg
keyPressDispatcher keyCode =
    Dict.get keyCode keyCodes
        |> Maybe.withDefault NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups keyPressDispatcher
        , Window.resizes WindowResizes
        ]


initFlow : List String
initFlow =
    [ "start", "first", "second", "async", "sync" ]


init : Result Errors Diagram -> Navigation.Location -> ( Model, Cmd Msg )
init diagram location =
    let
        flowStack =
            Flow.init initFlow

        model =
            Model diagram flowStack
    in
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ containerStyle ]
            [ Html.div [ explanationStyle ] [ viewExplanation model ]
            , Html.div [ diagramStyle ] [ viewDiagram model ]
            ]
        ]


containerStyle : Html.Attribute msg
containerStyle =
    style
        [ ( "display", "flex" )
        , ( "flex-flow", "row wrap" )
        , ( "justify-content", "space-around" )
        , ( "width", "100%" )
        , ( "background-color", "#dedede" )
        ]


explanationStyle : Html.Attribute msg
explanationStyle =
    style
        [ ( "order", "1" )
        , ( "width", "40%" )
        ]


diagramStyle : Html.Attribute msg
diagramStyle =
    style
        [ ( "order", "2" )
        , ( "width", "50%" )
        , ( "margin-top", "40px" )
        , ( "margin-bottom", "30px" )
        ]


viewExplanation : Model -> Html Msg
viewExplanation { flow } =
    case Flow.current flow of
        Nothing ->
            Html.div []
                [ Html.h1 [] [ Html.text "Sequence Diagram example" ]
                , Html.p [] [ Html.text "Use the following keys to navigate through the diagram:" ]
                , Html.ul []
                    [ Html.li [] [ Html.text "s - to start" ]
                    , Html.li [] [ Html.text "n - for the next next" ]
                    , Html.li [] [ Html.text "p - for a step back" ]
                    ]
                ]

        Just t ->
            case Dict.get t texts of
                Just txt ->
                    txt

                Nothing ->
                    Html.div []
                        [ Html.h1 [] [ Html.text "No explanation" ]
                        ]


texts : Dict.Dict String (Html Msg)
texts =
    let
        startText =
            Html.div []
                [ Html.h1 [] [ Html.text "Start of the sequence" ]
                , Html.p [] [ Html.text "Use client" ]
                ]

        firstText =
            Html.div []
                [ Html.h1 [] [ Html.text "API 1" ]
                , Html.p [] [ Html.text "Post something to API 1" ]
                ]

        secondText =
            Html.div []
                [ Html.h1 [] [ Html.text "API 2" ]
                , Html.p [] [ Html.text "Do the heavy lifting, multiple steps are involved" ]
                ]

        asyncText =
            Html.div []
                [ Html.h1 [] [ Html.text "Notify the Backend" ]
                , Html.p [] [ Html.text "Use POST to notify the Backend" ]
                ]

        syncText =
            Html.div []
                [ Html.h1 [] [ Html.text "Calc and Store" ]
                , Html.p [] [ Html.text "Notify client " ]
                , Html.p [] [ Html.text "Calculate " ]
                , Html.p [] [ Html.text "Store at backend" ]
                ]
    in
    [ ( "start", startText )
    , ( "first", firstText )
    , ( "second", secondText )
    , ( "async", asyncText )
    , ( "sync", syncText )
    ]
        |> Dict.fromList


viewDiagram : Model -> Html Msg
viewDiagram model =
    case model.diagram of
        Ok diagram ->
            Diagram.view diagram

        Err errors ->
            Html.div []
                [ Html.h2 [] [ Html.text "Oops" ]
                , Html.ul []
                    (List.map viewError errors)
                ]


viewError : String -> Html Msg
viewError error =
    Html.li [] [ Html.text error ]


app : Result Errors Diagram -> Program Never Model Msg
app rDiagram =
    Navigation.program
        NewLocation
        { init = init rDiagram
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


main : Program Never Model Msg
main =
    app Sequence.create
