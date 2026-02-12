module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Http
import IIIF exposing (requestManifest, v3PresentationContext)
import IIIF.Language exposing (Language(..), extractLabelFromLanguageMap)
import IIIF.Presentation exposing (IIIFManifest(..), toLabel, toSummary)


type alias Model =
    { label : String }


type Msg
    = GotManifest (Result Http.Error IIIFManifest)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { label = "Loading..." }
    , requestManifest GotManifest
        [ "application/ld+json;profile=" ++ v3PresentationContext ]
        "https://iiif.bodleian.ox.ac.uk/iiif/manifest/53fd0f29-d482-46e1-aa9d-37829b49987d.json"
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotManifest result ->
            case result of
                Ok manifest ->
                    let
                        labelText =
                            toLabel manifest
                                |> extractLabelFromLanguageMap Default

                        descriptionText =
                            Maybe.map (extractLabelFromLanguageMap Default) (toSummary manifest)
                                |> Maybe.withDefault "[No Summary]"
                    in
                    ( { model
                        | label = "Success! The manifest you loaded has a label of: " ++ labelText ++ " and a summary description of: " ++ descriptionText
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | label = "Failed to load manifest" }, Cmd.none )


view : Model -> Html Msg
view model =
    div [] [ text model.label ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
