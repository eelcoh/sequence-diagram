module Main exposing (..)

import Color
import Diagram exposing (Diagram, Errors, first, full, next, prev, rewind)
import Diagram.Attribute exposing (backgroundColour, caption, return, textColour)
import Dict
import Html exposing (Html)
import Keyboard
import Navigation
import Participant exposing (person, system)
import Sequence exposing (async, refSync, sequence, sync)
import Window


type alias Model =
    Result Errors Diagram


type Msg
    = Start
    | Next
    | Previous
    | End
    | NewLocation Navigation.Location
    | WindowResizes Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Start ->
            let
                diagram =
                    Result.map rewind model
            in
                ( diagram, Cmd.none )

        End ->
            let
                diagram =
                    Result.map full model
            in
                ( diagram, Cmd.none )

        Next ->
            let
                diagram =
                    Result.map next model
            in
                ( diagram, Cmd.none )

        Previous ->
            let
                diagram =
                    Result.map prev model
            in
                ( diagram, Cmd.none )

        NewLocation l ->
            ( model, Cmd.none )

        WindowResizes windowSize ->
            let
                diagram =
                    Result.map (\d -> Diagram.resize d windowSize) model
            in
                ( diagram, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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
            diagram
    in
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.h1 [] [ Html.text "Sequence Diagram example" ]
            , Html.p [] [ Html.text "Use the following keys to navigate through the diagram:" ]
            , Html.ul []
                [ Html.li [] [ Html.text "s - to start" ]
                , Html.li [] [ Html.text "n - for the next next" ]
                , Html.li [] [ Html.text "p - for a step back" ]
                , Html.li [] [ Html.text "f - for a full view" ]
                ]
            ]
        , viewDiagram model
        ]


viewDiagram : Model -> Html Msg
viewDiagram model =
    case model of
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
    let
        fg =
            Color.rgb 242 242 242

        bg =
            Color.rgb 255 98 0

        participants =
            [ person "customer" [ backgroundColour bg, textColour fg ]
            , system "app" [ backgroundColour bg, textColour fg, caption "client" ]
            , system "gateway" [ backgroundColour bg, textColour fg ]
            , system "api1" [ backgroundColour bg, textColour fg ]
            , system "api2" [ backgroundColour bg, textColour fg ]
            , system "backend" [ backgroundColour bg, textColour fg ]
            ]

        seq =
            sequence "customer"
                []
                [ sync "app"
                    [ caption "do something" ]
                    [ sync "api1" [ caption "post /something" ] []
                    , sync "api2"
                        [ caption "post /anything", return "thing" ]
                        [ async "backend" [ caption "post /this" ] []
                        , sync "app" [ caption "back" ] []
                        , sync "api2" [ caption "store" ] []
                        , sync "backend" [ caption "post /this/too" ] []
                        ]
                    , refSync "seq2" [ caption "REFSYNC" ]
                    ]
                ]

        seq2 =
            sequence "api1"
                []
                [ sync "api2" [ caption "api2 call" ] []
                , sync "backend" [ caption "backend call" ] []
                ]

        rDiagram =
            Diagram.create participants seq [ ( "seq2", seq2 ) ]
    in
        app rDiagram
