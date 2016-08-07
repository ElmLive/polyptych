module Main exposing (..)

import Html.App
import Html
import Html.Attributes


borderColor =
    "tan"


type alias Model =
    { canvas : Size
    , borderSize : Int
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
    , borderSize = 5
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


viewCanvas : Int -> Size -> Frame -> Html.Html Msg
viewCanvas borderSize size rootFrame =
    Html.div
        [ Html.Attributes.style
            [ ( "width", toString size.width ++ "px" )
            , ( "height", toString size.height ++ "px" )
            , ( "border", "2px solid black" )
            ]
        ]
        [ Html.div
            [ Html.Attributes.style
                [ ( "border", toString borderSize ++ "px solid " ++ borderColor )
                ]
            ]
            [ viewFrame borderSize
                { width = size.width - 2 * borderSize
                , height = size.height - 2 * borderSize
                }
                rootFrame
            ]
        ]


viewFrame : Int -> Size -> Frame -> Html.Html Msg
viewFrame borderSize size frame =
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
                [ viewFrame borderSize
                    { width = size.width
                    , height = topHeight
                    }
                    top
                , Html.div
                    [ Html.Attributes.style
                        [ ( "width", toString size.width ++ "px" )
                        , ( "height", toString borderSize ++ "px" )
                        , ( "background-color", borderColor )
                        ]
                    ]
                    []
                , viewFrame borderSize
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
        [ viewCanvas model.borderSize model.canvas model.frame
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
