module Sequences exposing (..)

import Diagram exposing (Diagram, Errors)
import Diagram.Participant exposing (person, system)
import Diagram.Sequence exposing (async, refSync, sequence, sync)
import Diagram.Attribute exposing (id, backgroundColour, caption, return, textColour)
import Color


create : Result Errors Diagram
create =
    let
        white =
            Color.rgb 242 242 242

        orange =
            Color.rgb 255 98 0

        blue =
            Color.rgb 96 166 218

        green =
            Color.rgb 52 150 81

        participants =
            [ person "customer" [ backgroundColour blue, textColour white ]
            , system "app" [ backgroundColour orange, textColour white, caption "client" ]
            , system "gateway" [ backgroundColour green, textColour white ]
            , system "api1" [ backgroundColour blue, textColour white ]
            , system "api2" [ backgroundColour blue, textColour white ]
            , system "backend" [ backgroundColour blue, textColour white ]
            ]

        seq =
            sequence "customer"
                [ id "0" ]
                [ sync "app"
                    [ id "1", caption "do something" ]
                    [ sync "api1" [ id "2", caption "post /something" ] []
                    , sync "api2"
                        [ id "3", caption "post /anything", return "thing" ]
                        [ async "backend"
                            [ id "4", caption "post /this" ]
                            [ sync "api2" [ id "5", caption "back" ] []
                            ]
                        , sync "api2" [ id "6", caption "store" ] []
                        , sync "backend" [ id "7", caption "post /this/too" ] []
                        ]
                    , refSync "seq2" [ id "8", caption "REFSYNC" ]
                    ]
                ]

        seq2 =
            sequence "api1"
                [ id "9" ]
                [ sync "api2" [ id "10", caption "api2 call" ] []
                , sync "backend" [ id "11", caption "backend call" ] []
                ]
    in
        Diagram.create participants seq [ ( "seq2", seq2 ) ]
