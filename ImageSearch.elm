module ImageSearch exposing (State, init, Msg, update, view)

import Html.App
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Http
import Task


type State
    = State
        { results : List PixabayImage
        }


init : State
init =
    State { results = [] }


type Msg
    = DoSearch
    | SearchFailure Http.Error
    | SearchSuccess PixabaySearchResponse
    | ImageSelected Image


type alias Image =
    { url : String }


type alias PixabaySearchResponse =
    { totalHits : Int
    , hits : List PixabayImage
    }


pixabaySearchResponseDecoder : Json.Decode.Decoder PixabaySearchResponse
pixabaySearchResponseDecoder =
    Json.Decode.object2 PixabaySearchResponse
        (Json.Decode.at [ "totalHits" ] Json.Decode.int)
        (Json.Decode.at [ "hits" ] (Json.Decode.list pixabayImageDecoder))


type alias PixabayImage =
    { preview : String
    , webFormat : String
    }


pixabayImageDecoder : Json.Decode.Decoder PixabayImage
pixabayImageDecoder =
    Json.Decode.object2 PixabayImage
        (Json.Decode.at [ "previewURL" ] Json.Decode.string)
        (Json.Decode.at [ "webformatURL" ] Json.Decode.string)


update : Msg -> State -> ( State, Cmd Msg, Maybe Image )
update msg (State state) =
    case msg of
        DoSearch ->
            ( State state
            , Http.get pixabaySearchResponseDecoder "https://pixabay.com/api/?key=3055468-47029136a9b3612c0dbc36058&q=yellow+flowers&image_type=photo&pretty=true"
                |> Task.perform SearchFailure SearchSuccess
            , Nothing
            )

        SearchSuccess data ->
            ( State { state | results = data.hits }
            , Cmd.none
            , Nothing
            )

        SearchFailure _ ->
            ( State state, Cmd.none, Nothing )

        ImageSelected image ->
            ( State state, Cmd.none, Just image )


view : State -> Html.Html Msg
view (State state) =
    Html.div []
        [ Html.button
            [ Html.Events.onClick DoSearch
            ]
            [ Html.text "Search" ]
        , Html.ul [] (List.map viewImage state.results)
        ]


viewImage : PixabayImage -> Html.Html Msg
viewImage image =
    Html.li []
        [ Html.img
            [ Html.Attributes.style
                [ ( "max-width", "100px" )
                , ( "max-height", "100px" )
                ]
            , Html.Attributes.src image.preview
            , Html.Events.onClick (ImageSelected { url = image.webFormat })
            ]
            []
        ]


main =
    Html.App.program
        { init = ( init, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update =
            \msg oldModel ->
                let
                    ( newModel, cmd, selectedImage ) =
                        update msg oldModel

                    _ =
                        Debug.log "selectedImage" selectedImage
                in
                    ( newModel, cmd )
        , view = view
        }
