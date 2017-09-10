module Main exposing (..)

import Diagram exposing (Diagram, Errors)
import Diagram.Internal.Render.Config exposing (calculateBase)
import Diagram.Navigate exposing (first, full, next, prev, rewind, zoom, zoomOut)
import Dict
import Html exposing (Html)
import Html.Attributes exposing (style)
import Keyboard
import Navigation
import Sequences
import Window


type alias Model =
    { diagram : Result Errors Diagram
    , currentId : Maybe String
    }


type Msg
    = Start
    | Next
    | Previous
    | End
    | Zoom
    | ZoomOut
    | NewLocation Navigation.Location
    | WindowResizes Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Start ->
            apply rewind model

        End ->
            apply full model

        Next ->
            apply next model

        Previous ->
            apply prev model

        Zoom ->
            apply zoom model

        ZoomOut ->
            apply zoomOut model

        WindowResizes windowSize ->
            let
                calcSize i =
                    toFloat i
                        |> (*) 0.4
                        |> floor

                newWidth =
                    calcSize windowSize.width

                newSize =
                    { windowSize | width = newWidth }

                newDiagram =
                    Result.map (\d -> (Diagram.resize d newSize)) model.diagram
            in
                ( { model | diagram = newDiagram }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        NewLocation _ ->
            ( model, Cmd.none )


apply : (Diagram -> ( Maybe String, Diagram )) -> Model -> ( Model, Cmd msg )
apply f model =
    let
        tempRes =
            Result.map f model.diagram

        newModel =
            case tempRes of
                Err errs ->
                    Model (Err errs) Nothing

                Ok ( mCurrentId, diagram ) ->
                    Model (Ok diagram) mCurrentId
    in
        ( newModel, Cmd.none )


keyCodes : Dict.Dict Int Msg
keyCodes =
    Dict.fromList
        [ ( 36, Start )
          -- home
        , ( 35, End )
          -- end
        , ( 13, Next )
          -- return
        , ( 32, Next )
          -- space
        , ( 39, Next )
          -- arrow right
        , ( 76, End )
          -- l
        , ( 82, Start )
          -- r for rewind
        , ( 83, Start )
          -- s for start
        , ( 68, Next )
          -- d
        , ( 70, End )
          -- f
        , ( 37, Previous )
          -- arrow left
        , ( 78, Next )
          -- n
        , ( 80, Previous )
          -- p
        , ( 72, Previous )
          -- h
        , ( 90, Zoom )
          -- z
        , ( 27, ZoomOut )
          -- Escape
        , ( 79, ZoomOut )
          -- o
        ]


keyPressDispatcher : Int -> Msg
keyPressDispatcher keyCode =
    Dict.get keyCode keyCodes
        |> Maybe.withDefault NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups (keyPressDispatcher)
        , Window.resizes WindowResizes
        ]


init : Result Errors Diagram -> Navigation.Location -> ( Model, Cmd Msg )
init diagram location =
    let
        model =
            Model diagram Nothing
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
viewExplanation { currentId } =
    case currentId of
        Nothing ->
            Html.div []
                [ Html.h1 [] [ Html.text "Sequence Diagram example" ]
                , Html.p [] [ Html.text "Use the following keys to navigate through the diagram:" ]
                , Html.ul []
                    [ Html.li [] [ Html.text "s - to start" ]
                    , Html.li [] [ Html.text "n - for the next next" ]
                    , Html.li [] [ Html.text "p - for a step back" ]
                    , Html.li [] [ Html.text "f - for a full view" ]
                    , Html.li [] [ Html.text "z - to zoom into a referred sequence" ]
                    , Html.li [] [ Html.text "o or Escape - to zoom out of a referred sequence" ]
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
    app Sequences.create
