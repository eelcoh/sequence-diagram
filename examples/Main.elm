module Main exposing (..)

import Sequence exposing (sequence, sync, async)
import Diagram exposing (first, next, prev, rewind, full, Diagram)
import Participant exposing (person, system)
import Diagram.Attribute exposing (backgroundColour, textColour, caption, return)
import Color
import Keyboard
import Navigation
import Window
import Dict
import Html exposing (Html)


{-
   App
-}


type alias Model =
    Diagram


type Msg
    = Start
    | Next
    | Previous
    | End
      --    | Select Hash
    | NewLocation Navigation.Location
    | WindowResizes Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Start ->
            ( rewind model, Cmd.none )

        End ->
            ( full model, Cmd.none )

        Next ->
            ( next model, Cmd.none )

        Previous ->
            ( prev model, Cmd.none )

        NewLocation l ->
            ( model, Cmd.none )

        WindowResizes windowSize ->
            let
                newModel =
                    Diagram.resize model windowSize
            in
                ( newModel, Cmd.none )

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


init : Diagram -> Navigation.Location -> ( Model, Cmd Msg )
init diagram location =
    let
        model =
            diagram
    in
        ( (first model), Cmd.none )


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
        , Diagram.view model
        ]


app : Diagram -> Program Never Model Msg
app diagram =
    Navigation.program
        NewLocation
        { init = init diagram
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
                    ]
                ]

        diagram =
            Diagram.create participants seq
    in
        app diagram
