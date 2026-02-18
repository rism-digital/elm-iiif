module IIIF.Internal.SharedDecoders exposing (behaviourDecoder, convertImageIdToImageUri, formatDecoder, imageContextListDecoder, imageContextMixedDecoder, imageContextStringDecoder, resourceTypeDecoder, viewingDirectionDecoder, viewingHintDecoder)

import IIIF.Image exposing (ImageUri, parseImageAddress)
import IIIF.ImageInfo exposing (IIIFInfo(..), InfoJson, WidthHeight, WidthHeightScale)
import IIIF.Internal.Contexts exposing (iiifV2ImageContextString, iiifV3ImageContextString)
import IIIF.Internal.Utilities exposing (optional, required)
import IIIF.Presentation exposing (Behavior(..), MediaFormats(..), ResourceTypes(..), ServiceTypes(..), ViewingDirection(..), ViewingHint(..), ViewingLayout(..), mediaFormatFromString, resourceTypeFromString, stringToBehavior, stringToViewingDirection, stringToViewingHint)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder)


viewingDirectionDecoder : Decoder ViewingDirection
viewingDirectionDecoder =
    Decode.string
        |> Decode.map stringToViewingDirection


viewingHintDecoder : Decoder ViewingLayout
viewingHintDecoder =
    Decode.string
        |> Decode.map (\str -> LayoutV2 (stringToViewingHint str))


behaviourDecoder : Decoder ViewingLayout
behaviourDecoder =
    Decode.map stringToBehavior Decode.string
        |> Decode.list
        |> Decode.map LayoutV3


convertImageIdToImageUri : String -> Decoder ImageUri
convertImageIdToImageUri idValue =
    case parseImageAddress idValue of
        Just url ->
            Decode.succeed url

        Nothing ->
            Decode.fail "Could not decode image Url"


formatDecoder : Decoder MediaFormats
formatDecoder =
    Decode.string
        |> Decode.map mediaFormatFromString


resourceTypeDecoder : Decoder ResourceTypes
resourceTypeDecoder =
    Decode.string
        |> Decode.map resourceTypeFromString


widthHeightDecoder : Decoder WidthHeight
widthHeightDecoder =
    Decode.succeed WidthHeight
        |> required "width" Decode.int
        |> required "height" Decode.int


widthHeightScaleDecoder : Decoder WidthHeightScale
widthHeightScaleDecoder =
    Decode.succeed WidthHeightScale
        |> required "width" Decode.int
        |> optional "height" (Decode.maybe Decode.int) Nothing
        |> required "scaleFactors" (Decode.list Decode.int)


iiifInfoDecoderWith : String -> Decoder InfoJson
iiifInfoDecoderWith idFieldName =
    Decode.succeed InfoJson
        |> required idFieldName (Decode.string |> Decode.andThen convertImageIdToImageUri)
        |> required "width" Decode.int
        |> required "height" Decode.int
        |> optional "sizes" (Decode.maybe (Decode.list widthHeightDecoder)) Nothing
        |> optional "tiles" (Decode.maybe (Decode.list widthHeightScaleDecoder)) Nothing


imageContextStringDecoder : String -> Decoder IIIFInfo
imageContextStringDecoder contextValue =
    if contextValue == iiifV3ImageContextString then
        Decode.map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id")

    else if contextValue == iiifV2ImageContextString then
        Decode.map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id")

    else
        Decode.fail ("Unknown IIIF Image Context value: " ++ contextValue)


imageContextListDecoder : List String -> Decoder IIIFInfo
imageContextListDecoder contextValues =
    if List.member iiifV3ImageContextString contextValues then
        Decode.map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id")

    else if List.member iiifV2ImageContextString contextValues then
        Decode.map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id")

    else
        Decode.fail ("Context list does not contain a known IIIF context value: " ++ String.join ", " contextValues)


imageContextMixedDecoder : Decoder IIIFInfo
imageContextMixedDecoder =
    Decode.list
        (Decode.oneOf
            [ Decode.string |> Decode.map Just
            , Decode.succeed Nothing
            ]
        )
        |> Decode.andThen (\maybeContext -> imageContextListDecoder (List.filterMap identity maybeContext))
