module IIIF.Decoders exposing (infoJsonDecoder, manifestDecoder, resourceDecoder)

{-| Decoder for IIIF Image API info.json.

@docs infoJsonDecoder, manifestDecoder, resourceDecoder

-}

import IIIF.ImageInfo exposing (IIIFInfo)
import IIIF.Internal.CoreDecoders exposing (contextListDecoder, contextStringDecoder, resourceContextListDecoder, resourceContextStringDecoder)
import IIIF.Internal.SharedDecoders exposing (imageContextListDecoder, imageContextMixedDecoder, imageContextStringDecoder)
import IIIF.Presentation exposing (IIIFManifest, IIIFResource)
import Json.Decode exposing (Decoder, andThen, field, list, oneOf, string)


infoJsonDecoder : Decoder IIIFInfo
infoJsonDecoder =
    oneOf
        [ field "@context" string
            |> andThen imageContextStringDecoder
        , field "@context" (list string)
            |> andThen imageContextListDecoder
        , field "@context" imageContextMixedDecoder
        ]


{-| Decoder for a IIIF manifest.
-}
manifestDecoder : Decoder IIIFManifest
manifestDecoder =
    oneOf
        [ field "@context" string
            |> andThen contextStringDecoder
        , field "@context" (list string)
            |> andThen contextListDecoder
        ]


{-| Decoder for a top-level IIIF resource.
-}
resourceDecoder : Decoder IIIFResource
resourceDecoder =
    oneOf
        [ field "@context" string
            |> andThen resourceContextStringDecoder
        , field "@context" (list string)
            |> andThen resourceContextListDecoder
        ]
