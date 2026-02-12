module IIIF.Internal.SharedDecoders exposing (behaviourDecoder, convertImageIdToImageUri, formatDecoder, imageContextListDecoder, imageContextMixedDecoder, imageContextMixedListDecoder, imageContextStringDecoder, resourceTypeDecoder, serviceTypeDecoder, viewingDirectionDecoder, viewingHintDecoder)

import IIIF.Image exposing (ImageUri, parseImageAddress)
import IIIF.ImageInfo exposing (IIIFInfo(..), InfoJson, WidthHeight, WidthHeightScale)
import IIIF.Internal.Contexts exposing (iiifV2ImageContextString, iiifV3ImageContextString)
import IIIF.Internal.Utilities exposing (optional, required)
import IIIF.Presentation exposing (Behavior(..), MediaFormats(..), ResourceTypes(..), ServiceTypes(..), ViewingDirection(..), ViewingHint(..), ViewingLayout(..), mediaFormatFromString, resourceTypeFromString, stringToBehavior, stringToServiceType, stringToViewingDirection, stringToViewingHint)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder)


viewingDirectionDecoder : Decoder ViewingDirection
viewingDirectionDecoder =
    Decode.string
        |> Decode.andThen viewingDirectionValueConverter


viewingDirectionValueConverter : String -> Decoder ViewingDirection
viewingDirectionValueConverter direction =
    stringToViewingDirection direction
        |> Decode.succeed


viewingHintDecoder : Decoder ViewingLayout
viewingHintDecoder =
    Decode.string
        |> Decode.andThen viewingHintValueConverter
        |> Decode.map LayoutV2


viewingHintValueConverter : String -> Decoder ViewingHint
viewingHintValueConverter hint =
    stringToViewingHint hint
        |> Decode.succeed


behaviourDecoder : Decoder ViewingLayout
behaviourDecoder =
    Decode.andThen behaviourValueConverter Decode.string
        |> Decode.list
        |> Decode.map LayoutV3


behaviourValueConverter : String -> Decoder Behavior
behaviourValueConverter behavior =
    stringToBehavior behavior
        |> Decode.succeed


convertImageIdToImageUri : String -> Decoder ImageUri
convertImageIdToImageUri idValue =
    case parseImageAddress idValue of
        Just url ->
            Decode.succeed url

        Nothing ->
            Decode.fail "Could not decode image Url"


serviceTypeDecoder : String -> Decoder ServiceTypes
serviceTypeDecoder val =
    stringToServiceType val
        |> Decode.succeed


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


imageContextMixedDecoder : Decoder (List (Maybe String))
imageContextMixedDecoder =
    Decode.list
        (Decode.oneOf
            [ Decode.string |> Decode.map Just
            , Decode.succeed Nothing
            ]
        )


imageContextMixedListDecoder : List (Maybe String) -> Decoder IIIFInfo
imageContextMixedListDecoder maybeContext =
    List.filterMap identity maybeContext
        |> imageContextListDecoder
