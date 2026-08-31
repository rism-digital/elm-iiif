module IIIF.Annotation exposing
    ( AnnotationSelector(..), AnnotationTarget, AnnotationBody, Annotation
    , decodePage
    )

{-| IIIF Presentation annotation types and decoders.

This module accepts annotation pages from both Presentation API 2 and 3. It
decodes the region selectors that image viewers can render: media-fragment
rectangles and SVG selectors.


# Types

@docs AnnotationSelector, AnnotationTarget, AnnotationBody, Annotation


# Decoding

@docs decodePage

-}

import Json.Decode as Decode exposing (Decoder)


{-| A selector identifying a region of a canvas.
-}
type AnnotationSelector
    = Rectangle Float Float Float Float
    | Svg String


{-| The canvas and region targeted by an annotation.
-}
type alias AnnotationTarget =
    { source : Maybe String
    , selector : AnnotationSelector
    }


{-| The textual content of an annotation body.
-}
type alias AnnotationBody =
    { value : String
    , format : Maybe String
    , language : Maybe String
    }


{-| A Web Annotation associated with a IIIF canvas.
-}
type alias Annotation =
    { id : String
    , target : AnnotationTarget
    , body : AnnotationBody
    , motivation : Maybe String
    }


{-| Decode a IIIF AnnotationPage, or a bare list of annotations.

Presentation API 3 pages use `items`; Presentation API 2 pages use
`resources`.

-}
decodePage : Decoder (List Annotation)
decodePage =
    Decode.oneOf
        [ Decode.field "resources" (Decode.list annotationDecoder)
        , Decode.field "items" (Decode.list annotationDecoder)
        , Decode.list annotationDecoder
        ]


annotationDecoder : Decoder Annotation
annotationDecoder =
    Decode.map4
        (\id target body motivation ->
            { id = id
            , target = target
            , body = body
            , motivation = motivation
            }
        )
        idDecoder
        (Decode.oneOf [ Decode.field "target" annotationTargetDecoder, Decode.field "on" annotationTargetDecoder ])
        annotationBodyDecoder
        (Decode.maybe (Decode.field "motivation" Decode.string))


idDecoder : Decoder String
idDecoder =
    Decode.oneOf [ Decode.field "id" Decode.string, Decode.field "@id" Decode.string, Decode.succeed "" ]


annotationTargetDecoder : Decoder AnnotationTarget
annotationTargetDecoder =
    Decode.oneOf
        [ Decode.string |> Decode.andThen targetFromString
        , Decode.map2
            (\source selector -> { source = source, selector = selector })
            sourceDecoder
            (Decode.field "selector" selectorDecoder)
        ]


sourceDecoder : Decoder (Maybe String)
sourceDecoder =
    Decode.oneOf
        [ Decode.field "source" (Decode.oneOf [ Decode.string, sourceObjectDecoder ]) |> Decode.map Just
        , Decode.succeed Nothing
        ]


sourceObjectDecoder : Decoder String
sourceObjectDecoder =
    Decode.oneOf
        [ Decode.field "id" Decode.string
        , Decode.field "@id" Decode.string
        ]


targetFromString : String -> Decoder AnnotationTarget
targetFromString target =
    case String.split "#xywh=" target of
        [ source, coordinates ] ->
            rectangleFromString coordinates
                |> Decode.map (\selector -> { source = Just source, selector = selector })

        _ ->
            Decode.fail "Annotation target has no xywh selector"


selectorDecoder : Decoder AnnotationSelector
selectorDecoder =
    Decode.oneOf
        [ svgSelectorDecoder
        , Decode.field "value" Decode.string |> Decode.andThen rectangleFromString
        , Decode.field "default" (Decode.lazy (\_ -> selectorDecoder))
        , Decode.field "item" (Decode.lazy (\_ -> selectorDecoder))
        ]


svgSelectorDecoder : Decoder AnnotationSelector
svgSelectorDecoder =
    Decode.map2 Tuple.pair selectorTypeDecoder (Decode.field "value" Decode.string)
        |> Decode.andThen
            (\( selectorType, value ) ->
                if String.toLower selectorType == "svgselector" || String.toLower selectorType == "oa:svgselector" then
                    Decode.succeed (Svg value)

                else
                    Decode.fail "Not an SVG selector"
            )


selectorTypeDecoder : Decoder String
selectorTypeDecoder =
    Decode.oneOf [ Decode.field "type" Decode.string, Decode.field "@type" Decode.string ]


rectangleFromString : String -> Decoder AnnotationSelector
rectangleFromString value =
    let
        coordinates =
            if String.startsWith "xywh=" value then
                String.dropLeft 5 value

            else
                value
    in
    case String.split "," coordinates |> List.map String.toFloat of
        [ Just x, Just y, Just width, Just height ] ->
            if width > 0 && height > 0 then
                Decode.succeed (Rectangle x y width height)

            else
                Decode.fail "Annotation rectangle must have a positive size"

        _ ->
            Decode.fail "Invalid xywh selector"


annotationBodyDecoder : Decoder AnnotationBody
annotationBodyDecoder =
    Decode.oneOf
        [ Decode.field "body" bodyValueDecoder
        , Decode.field "resource" bodyValueDecoder
        , Decode.succeed emptyBody
        ]


bodyValueDecoder : Decoder AnnotationBody
bodyValueDecoder =
    Decode.oneOf
        [ Decode.map3
            (\value format language -> { value = value, format = format, language = language })
            (Decode.oneOf [ Decode.field "value" Decode.string, Decode.field "chars" Decode.string ])
            (Decode.maybe (Decode.field "format" Decode.string))
            (Decode.maybe (Decode.field "language" Decode.string))
        , Decode.list (Decode.lazy (\_ -> bodyValueDecoder)) |> Decode.map (List.head >> Maybe.withDefault emptyBody)
        , Decode.string |> Decode.map bodyFromString
        ]


bodyFromString : String -> AnnotationBody
bodyFromString value =
    { value = value, format = Nothing, language = Nothing }


emptyBody : AnnotationBody
emptyBody =
    bodyFromString ""
