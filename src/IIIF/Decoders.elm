module IIIF.Decoders exposing (..)

{-| Decoder for IIIF Image API info.json.

@docs infoJsonDecoder, manifestDecoder, resourceDecoder

-}

import IIIF.ImageInfo exposing (IIIFInfo)
import IIIF.Internal.CoreDecoders exposing (contextListDecoder, contextStringDecoder, resourceContextListDecoder, resourceContextStringDecoder)
import IIIF.Internal.SharedDecoders exposing (imageContextListDecoder, imageContextMixedDecoder, imageContextMixedListDecoder, imageContextStringDecoder)
import IIIF.Presentation exposing (IIIFManifest, IIIFResource)
import Json.Decode as Decode exposing (Decoder)


infoJsonDecoder : Decoder IIIFInfo
infoJsonDecoder =
    Decode.oneOf
        [ Decode.field "@context" Decode.string
            |> Decode.andThen imageContextStringDecoder
        , Decode.field "@context" (Decode.list Decode.string)
            |> Decode.andThen imageContextListDecoder
        , Decode.field "@context" imageContextMixedDecoder
            |> Decode.andThen imageContextMixedListDecoder
        ]


{-| Decoder for a IIIF manifest.
-}
manifestDecoder : Decoder IIIFManifest
manifestDecoder =
    Decode.oneOf
        [ Decode.field "@context" Decode.string
            |> Decode.andThen contextStringDecoder
        , Decode.field "@context" (Decode.list Decode.string)
            |> Decode.andThen contextListDecoder
        ]


{-| Decoder for a top-level IIIF resource.
-}
resourceDecoder : Decoder IIIFResource
resourceDecoder =
    Decode.oneOf
        [ Decode.field "@context" Decode.string
            |> Decode.andThen resourceContextStringDecoder
        , Decode.field "@context" (Decode.list Decode.string)
            |> Decode.andThen resourceContextListDecoder
        ]
