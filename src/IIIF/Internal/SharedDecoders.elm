module IIIF.Internal.SharedDecoders exposing (behaviourDecoder, convertImageIdToImageUri, convertStaticImageIdToImageUri, convertThumbnailImageIdToImageUri, formatDecoder, imageContextListDecoder, imageContextMixedDecoder, imageContextStringDecoder, resourceTypeDecoder, thumbnailDecoder, viewingDirectionDecoder, viewingHintDecoder)

import IIIF.Image exposing (ImageUri(..), imageUriToInfoUri, parseImageAddress, staticImageUriFromUrl)
import IIIF.ImageInfo exposing (IIIFInfo(..), InfoJson, WidthHeight, WidthHeightScale)
import IIIF.Internal.Contexts exposing (contextMatches, iiifV2ImageContextString, iiifV3ImageContextString)
import IIIF.Internal.Utilities exposing (optional, required)
import IIIF.Presentation exposing (MediaFormats, ResourceTypes, ViewingDirection, ViewingLayout(..), mediaFormatFromString, resourceTypeFromString, stringToBehavior, stringToViewingDirection, stringToViewingHint)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode exposing (Decoder, andThen, fail, int, list, map, maybe, oneOf, string, succeed)
import Url


viewingDirectionDecoder : Decoder ViewingDirection
viewingDirectionDecoder =
    string
        |> map stringToViewingDirection


viewingHintDecoder : Decoder ViewingLayout
viewingHintDecoder =
    string
        |> map (\str -> LayoutV2 (stringToViewingHint str))


behaviourDecoder : Decoder ViewingLayout
behaviourDecoder =
    map stringToBehavior string
        |> list
        |> map LayoutV3


convertImageIdToImageUri : String -> Decoder ImageUri
convertImageIdToImageUri idValue =
    case parseImageAddress idValue of
        Just (StaticImageUri params) ->
            succeed (InfoUri params)

        Just url ->
            succeed (imageUriToInfoUri url)

        Nothing ->
            fail "Could not decode image Url"


convertStaticImageIdToImageUri : String -> Decoder ImageUri
convertStaticImageIdToImageUri idValue =
    case parseImageAddress idValue of
        Just (StaticImageUri params) ->
            succeed (StaticImageUri params)

        _ ->
            fail "Could not decode static image Url"


convertThumbnailImageIdToImageUri : String -> Decoder ImageUri
convertThumbnailImageIdToImageUri idValue =
    case Url.fromString idValue of
        Just url ->
            succeed (staticImageUriFromUrl url)

        Nothing ->
            fail "Could not decode static image Url"


formatDecoder : Decoder MediaFormats
formatDecoder =
    string
        |> map mediaFormatFromString


resourceTypeDecoder : Decoder ResourceTypes
resourceTypeDecoder =
    string
        |> map resourceTypeFromString


thumbnailDecoder : Decoder a -> Decoder (Maybe a)
thumbnailDecoder imageDecoder =
    oneOf
        [ list imageDecoder |> map List.head
        , imageDecoder |> map Just
        ]


widthHeightDecoder : Decoder WidthHeight
widthHeightDecoder =
    succeed WidthHeight
        |> required "width" int
        |> required "height" int


widthHeightScaleDecoder : Decoder WidthHeightScale
widthHeightScaleDecoder =
    succeed WidthHeightScale
        |> required "width" int
        |> optional "height" (maybe int) Nothing
        |> required "scaleFactors" (list int)


iiifInfoDecoderWith : String -> Decoder InfoJson
iiifInfoDecoderWith idFieldName =
    succeed InfoJson
        |> required idFieldName (string |> andThen convertImageIdToImageUri)
        |> required "width" int
        |> required "height" int
        |> optional "sizes" (maybe (list widthHeightDecoder)) Nothing
        |> optional "tiles" (maybe (list widthHeightScaleDecoder)) Nothing


imageContextStringDecoder : String -> Decoder IIIFInfo
imageContextStringDecoder contextValue =
    if contextMatches iiifV3ImageContextString contextValue then
        map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id")

    else if contextMatches iiifV2ImageContextString contextValue then
        map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id")

    else
        fail ("Unknown IIIF Image Context value: " ++ contextValue)


imageContextListDecoder : List String -> Decoder IIIFInfo
imageContextListDecoder contextValues =
    if List.any (contextMatches iiifV3ImageContextString) contextValues then
        map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id")

    else if List.any (contextMatches iiifV2ImageContextString) contextValues then
        map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id")

    else
        fail ("Context list does not contain a known IIIF context value: " ++ String.join ", " contextValues)


imageContextMixedDecoder : Decoder IIIFInfo
imageContextMixedDecoder =
    list
        (oneOf
            [ string |> map Just
            , succeed Nothing
            ]
        )
        |> andThen (\maybeContext -> imageContextListDecoder (List.filterMap identity maybeContext))
