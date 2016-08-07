module Main exposing (..)

import Html.App
import Html
import Html.Attributes


type alias Model =
    ()


initialModel : Model
initialModel =
    ()


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


viewCanvas : Html.Html Msg
viewCanvas =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "250px" )
            , ( "height", "250px" )
            , ( "border", "2px solid black" )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "height", "250px" )
                , ( "background-image", "url(https://pixabay.com/static/uploads/photo/2015/10/18/11/58/beetles-994211_960_720.jpg)" )
                , ( "background-size", "auto 250px" )
                ]
            ]
            []
        ]


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "padding", "8px" ) ]
        ]
        [ viewCanvas
        , Html.hr [] []
        , Html.text <| toString model
        ]


main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }
