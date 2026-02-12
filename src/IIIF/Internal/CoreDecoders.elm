module IIIF.Internal.CoreDecoders exposing (contextListDecoder, contextStringDecoder, resourceContextListDecoder, resourceContextStringDecoder)

import IIIF.Internal.Contexts exposing (iiifV2PresentationContextString, iiifV3PresentationContextString)
import IIIF.Internal.V2PresentationDecoders exposing (v2ResourceTypeDecoder, v2iiifManifestDecoder)
import IIIF.Internal.V3PresentationDecoders exposing (v3ResourceTypeDecoder, v3iiifManifestDecoder)
import IIIF.Presentation exposing (IIIFManifest(..), IIIFResource(..))
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder)


contextStringDecoder : String -> Decoder IIIFManifest
contextStringDecoder contextValue =
    if contextValue == iiifV3PresentationContextString then
        Decode.map (IIIFManifest IIIFV3) v3iiifManifestDecoder

    else if contextValue == iiifV2PresentationContextString then
        Decode.map (IIIFManifest IIIFV2) v2iiifManifestDecoder

    else
        Decode.fail ("Unknown IIIF Version: " ++ contextValue)


contextListDecoder : List String -> Decoder IIIFManifest
contextListDecoder contextValues =
    if List.member iiifV3PresentationContextString contextValues then
        Decode.map (IIIFManifest IIIFV3) v3iiifManifestDecoder

    else if List.member iiifV2PresentationContextString contextValues then
        Decode.map (IIIFManifest IIIFV2) v2iiifManifestDecoder

    else
        Decode.fail ("Unknown IIIF Version in context values: " ++ String.join ", " contextValues)


resourceContextStringDecoder : String -> Decoder IIIFResource
resourceContextStringDecoder contextValue =
    if contextValue == iiifV3PresentationContextString then
        v3ResourceTypeDecoder

    else if contextValue == iiifV2PresentationContextString then
        v2ResourceTypeDecoder

    else
        Decode.fail ("Unknown resource context value: " ++ contextValue)


resourceContextListDecoder : List String -> Decoder IIIFResource
resourceContextListDecoder contextValues =
    if List.member iiifV3PresentationContextString contextValues then
        v3ResourceTypeDecoder

    else if List.member iiifV2PresentationContextString contextValues then
        v2ResourceTypeDecoder

    else
        Decode.fail ("Context values contain an unknown IIIF version: " ++ String.join ", " contextValues)
