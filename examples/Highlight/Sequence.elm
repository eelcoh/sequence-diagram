module Sequence exposing (create)

import Color
import Diagram exposing (Diagram, Errors)
import Diagram.Attribute exposing (backgroundColour, caption, return, tag, textColour)
import Diagram.Participant exposing (person, system)
import Diagram.Sequence exposing (async, refSync, sequence, sync)


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
                [ tag "start" ]
                [ sync "app"
                    [ tag "start", caption "do something" ]
                    [ sync "api1" [ tag "first", caption "post /something" ] []
                    , sync "api2"
                        [ tag "second", caption "post /anything", return "thing" ]
                        [ async "backend"
                            [ tag "second", tag "async", caption "post /this" ]
                            [ sync "api2" [ tag "second", tag "async", caption "back" ] []
                            ]
                        , sync "api2" [ tag "second", tag "sync", caption "store" ] []
                        , sync "backend" [ tag "second", tag "sync", caption "post /this/too" ] []
                        ]
                    ]
                ]
    in
    Diagram.create participants seq []
