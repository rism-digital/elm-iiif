module IIIF.Internal.CoreDecoders exposing (contextListDecoder, contextStringDecoder, resourceContextListDecoder, resourceContextStringDecoder)

import IIIF.Internal.Contexts exposing (contextMatches, iiifV2PresentationContextString, iiifV3PresentationContextString)
import IIIF.Internal.V2PresentationDecoders exposing (v2ResourceTypeDecoder, v2iiifManifestDecoder)
import IIIF.Internal.V3PresentationDecoders exposing (v3ResourceTypeDecoder, v3iiifManifestDecoder)
import IIIF.Presentation exposing (IIIFManifest(..), IIIFResource)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode exposing (Decoder, fail, map)


contextStringDecoder : String -> Decoder IIIFManifest
contextStringDecoder contextValue =
    if contextMatches iiifV3PresentationContextString contextValue then
        map (IIIFManifest IIIFV3) v3iiifManifestDecoder

    else if contextMatches iiifV2PresentationContextString contextValue then
        map (IIIFManifest IIIFV2) v2iiifManifestDecoder

    else
        fail ("Unknown IIIF Version: " ++ contextValue)


contextListDecoder : List String -> Decoder IIIFManifest
contextListDecoder contextValues =
    if List.any (contextMatches iiifV3PresentationContextString) contextValues then
        map (IIIFManifest IIIFV3) v3iiifManifestDecoder

    else if List.any (contextMatches iiifV2PresentationContextString) contextValues then
        map (IIIFManifest IIIFV2) v2iiifManifestDecoder

    else
        fail ("Unknown IIIF Version in context values: " ++ String.join ", " contextValues)


resourceContextStringDecoder : String -> Decoder IIIFResource
resourceContextStringDecoder contextValue =
    if contextMatches iiifV3PresentationContextString contextValue then
        v3ResourceTypeDecoder

    else if contextMatches iiifV2PresentationContextString contextValue then
        v2ResourceTypeDecoder

    else
        fail ("Unknown resource context value: " ++ contextValue)


resourceContextListDecoder : List String -> Decoder IIIFResource
resourceContextListDecoder contextValues =
    if List.any (contextMatches iiifV3PresentationContextString) contextValues then
        v3ResourceTypeDecoder

    else if List.any (contextMatches iiifV2PresentationContextString) contextValues then
        v2ResourceTypeDecoder

    else
        fail ("Context values contain an unknown IIIF version: " ++ String.join ", " contextValues)
