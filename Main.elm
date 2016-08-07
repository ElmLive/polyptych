module Main exposing (..)

import Html.App
import Html
import Html.Attributes


type alias Model =
    { canvas : Size
    , frame : Frame
    }


type alias Size =
    { width : Int, height : Int }


type Frame
    = SingleImage { url : String }
    | HorizontalSplit
        { top : Frame
        , topHeight : Int
        , bottom : Frame
        }


initialModel : Model
initialModel =
    { canvas = { width = 250, height = 250 }
    , frame =
        HorizontalSplit
            { top = SingleImage { url = "https://i.imgur.com/gt5lnkS.jpg" }
            , topHeight = 80
            , bottom = SingleImage { url = "http://imgur.com/4mYf6Jh.jpg" }
            }
    }


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


viewCanvas : Size -> Frame -> Html.Html Msg
viewCanvas size rootFrame =
    Html.div
        [ Html.Attributes.style
            [ ( "width", toString size.width ++ "px" )
            , ( "height", toString size.height ++ "px" )
            , ( "border", "2px solid black" )
            ]
        ]
        [ viewFrame size rootFrame
        ]


viewFrame : Size -> Frame -> Html.Html Msg
viewFrame size frame =
    case frame of
        SingleImage { url } ->
            Html.div
                [ Html.Attributes.style
                    [ ( "height", toString size.height ++ "px" )
                    , ( "width", toString size.width ++ "px" )
                    , ( "background-image", "url(" ++ url ++ ")" )
                    , ( "background-size", "auto " ++ toString size.height ++ "px" )
                    ]
                ]
                []

        HorizontalSplit { top, topHeight, bottom } ->
            Html.div []
                [ viewFrame
                    { width = size.width
                    , height = topHeight
                    }
                    top
                , viewFrame
                    { width = size.width
                    , height = size.height - topHeight
                    }
                    bottom
                ]


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.style [ ( "padding", "8px" ) ]
        ]
        [ viewCanvas model.canvas model.frame
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
