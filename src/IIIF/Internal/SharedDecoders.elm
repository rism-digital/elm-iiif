module IIIF.Internal.SharedDecoders exposing (behaviourDecoder, convertImageIdToImageUri, convertStaticImageIdToImageUri, convertThumbnailImageIdToImageUri, formatDecoder, imageContextListDecoder, imageContextMixedDecoder, imageContextStringDecoder, resourceTypeDecoder, thumbnailDecoder, viewingDirectionDecoder, viewingHintDecoder)

import IIIF.Image exposing (ImageUri(..), imageUriToInfoUri, parseImageAddress, staticImageUriFromUrl)
import IIIF.ImageInfo exposing (ComplianceLevel(..), IIIFInfo(..), InfoJson, InfoProfile, WidthHeight, WidthHeightScale)
import IIIF.Internal.Contexts exposing (contextMatches, iiifV2ImageContextString, iiifV3ImageContextString, isV1ImageContext)
import IIIF.Internal.Utilities exposing (custom, optional, required)
import IIIF.Presentation exposing (MediaFormats, ResourceTypes, ViewingDirection, ViewingLayout(..), mediaFormatFromString, resourceTypeFromString, stringToBehavior, stringToViewingDirection, stringToViewingHint)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder, Value, andThen, fail, field, int, keyValuePairs, list, map, maybe, oneOf, string, succeed, value)
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


iiifInfoDecoderWith : String -> Decoder (Maybe InfoProfile) -> Decoder InfoJson
iiifInfoDecoderWith idFieldName profileDecoder =
    succeed InfoJson
        |> required idFieldName (string |> andThen convertImageIdToImageUri)
        |> required "width" int
        |> required "height" int
        |> optional "sizes" (maybe (list widthHeightDecoder)) Nothing
        |> optional "tiles" (maybe (list widthHeightScaleDecoder)) Nothing
        |> custom profileDecoder


v1InfoDecoder : Decoder InfoJson
v1InfoDecoder =
    succeed InfoJson
        |> required "@id" (string |> andThen convertImageIdToImageUri)
        |> required "width" int
        |> required "height" int
        |> custom (succeed Nothing)
        |> custom v1TilesDecoder
        |> custom v1InfoProfileDecoder


v1TilesDecoder : Decoder (Maybe (List WidthHeightScale))
v1TilesDecoder =
    Decode.map3
        (\maybeWidth maybeHeight maybeScaleFactors ->
            case ( maybeWidth, maybeScaleFactors ) of
                ( Just width, Just scaleFactors ) ->
                    Just
                        [ { width = width
                          , height = maybeHeight
                          , scaleFactors = scaleFactors
                          }
                        ]

                _ ->
                    Nothing
        )
        (strictOptionalField "tile_width" int)
        (strictOptionalField "tile_height" int)
        (strictOptionalField "scale_factors" (list int))


v1InfoProfileDecoder : Decoder (Maybe InfoProfile)
v1InfoProfileDecoder =
    Decode.map3
        (\maybeCompliance formats qualities ->
            Maybe.map
                (\complianceLevel ->
                    { complianceLevel = complianceLevel
                    , formats = formats
                    , qualities = qualities
                    , supports = Nothing
                    , maxWidth = Nothing
                    , maxHeight = Nothing
                    , maxArea = Nothing
                    }
                )
                maybeCompliance
        )
        (strictOptionalField "profile" (string |> map complianceLevelFromString))
        (strictOptionalField "formats" (list string))
        (strictOptionalField "qualities" (list string))


complianceLevelFromString : String -> ComplianceLevel
complianceLevelFromString value =
    case value of
        "http://iiif.io/api/image/2/level0.json" ->
            Level0

        "http://iiif.io/api/image/2/level1.json" ->
            Level1

        "http://iiif.io/api/image/2/level2.json" ->
            Level2

        "http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level0" ->
            Level0

        "http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level1" ->
            Level1

        "http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level2" ->
            Level2

        "https://iiif.io/api/image/2/level0.json" ->
            Level0

        "https://iiif.io/api/image/2/level1.json" ->
            Level1

        "https://iiif.io/api/image/2/level2.json" ->
            Level2

        "https://library.stanford.edu/iiif/image-api/1.1/compliance.html#level0" ->
            Level0

        "https://library.stanford.edu/iiif/image-api/1.1/compliance.html#level1" ->
            Level1

        "https://library.stanford.edu/iiif/image-api/1.1/compliance.html#level2" ->
            Level2

        "level0" ->
            Level0

        "level1" ->
            Level1

        "level2" ->
            Level2

        _ ->
            UnknownLevel value


emptyInfoProfile : ComplianceLevel -> InfoProfile
emptyInfoProfile complianceLevel =
    { complianceLevel = complianceLevel
    , formats = Nothing
    , qualities = Nothing
    , supports = Nothing
    , maxWidth = Nothing
    , maxHeight = Nothing
    , maxArea = Nothing
    }


v2InfoProfileDetailsDecoder : Decoder (InfoProfile -> InfoProfile)
v2InfoProfileDetailsDecoder =
    keyValuePairs value
        |> andThen (\_ -> v2InfoProfileDetailsObjectDecoder)


v2InfoProfileDetailsObjectDecoder : Decoder (InfoProfile -> InfoProfile)
v2InfoProfileDetailsObjectDecoder =
    succeed
        (\formats qualities supports maxWidth maxHeight maxArea profile ->
            { profile
                | formats = formats
                , qualities = qualities
                , supports = supports
                , maxWidth = maxWidth
                , maxHeight = maxHeight
                , maxArea = maxArea
            }
        )
        |> custom (strictOptionalField "formats" (list string))
        |> custom (strictOptionalField "qualities" (list string))
        |> custom (strictOptionalField "supports" (list string))
        |> custom (strictOptionalField "maxWidth" int)
        |> custom (strictOptionalField "maxHeight" int)
        |> custom (strictOptionalField "maxArea" int)


v2InfoProfileDecoder : Decoder InfoProfile
v2InfoProfileDecoder =
    oneOf
        [ string |> map (\value -> emptyInfoProfile (complianceLevelFromString value))
        , list value |> andThen v2InfoProfileArrayDecoder
        ]


v2InfoProfileArrayDecoder : List Value -> Decoder InfoProfile
v2InfoProfileArrayDecoder values =
    case values of
        [ rawLevel ] ->
            decodeProfileValue string rawLevel
                |> map (\level -> emptyInfoProfile (complianceLevelFromString level))

        [ rawLevel, rawDetails ] ->
            Decode.map2
                (\level applyDetails -> applyDetails (emptyInfoProfile (complianceLevelFromString level)))
                (decodeProfileValue string rawLevel)
                (decodeProfileValue v2InfoProfileDetailsDecoder rawDetails)

        _ ->
            fail "Expected a v2 profile array with one or two values"


decodeProfileValue : Decoder a -> Value -> Decoder a
decodeProfileValue decoder rawValue =
    case Decode.decodeValue decoder rawValue of
        Ok decoded ->
            succeed decoded

        Err error ->
            fail (Decode.errorToString error)


v3InfoProfileDecoder : Decoder InfoProfile
v3InfoProfileDecoder =
    succeed InfoProfile
        |> required "profile" (string |> map complianceLevelFromString)
        |> custom (strictOptionalField "extraFormats" (list string))
        |> custom (strictOptionalField "extraQualities" (list string))
        |> custom (strictOptionalField "extraFeatures" (list string))
        |> custom (strictOptionalField "maxWidth" int)
        |> custom (strictOptionalField "maxHeight" int)
        |> custom (strictOptionalField "maxArea" int)


strictOptionalField : String -> Decoder a -> Decoder (Maybe a)
strictOptionalField fieldName decoder =
    value
        |> andThen
            (\object ->
                if objectHasField fieldName object then
                    decodeProfileValue (field fieldName decoder) object |> map Just

                else
                    succeed Nothing
            )


strictOptionalObject : String -> Decoder a -> Decoder (Maybe a)
strictOptionalObject fieldName decoder =
    value
        |> andThen
            (\object ->
                if objectHasField fieldName object then
                    decodeProfileValue decoder object |> map Just

                else
                    succeed Nothing
            )


objectHasField : String -> Value -> Bool
objectHasField fieldName object =
    case Decode.decodeValue (keyValuePairs value) object of
        Ok fields ->
            List.any (\( name, _ ) -> name == fieldName) fields

        Err _ ->
            False


imageContextStringDecoder : String -> Decoder IIIFInfo
imageContextStringDecoder contextValue =
    if contextMatches iiifV3ImageContextString contextValue then
        map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id" (strictOptionalObject "profile" v3InfoProfileDecoder))

    else if contextMatches iiifV2ImageContextString contextValue then
        map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id" (strictOptionalField "profile" v2InfoProfileDecoder))

    else if isV1ImageContext contextValue then
        map (IIIFInfo IIIFV1) v1InfoDecoder

    else
        fail ("Unknown IIIF Image Context value: " ++ contextValue)


imageContextListDecoder : List String -> Decoder IIIFInfo
imageContextListDecoder contextValues =
    if List.any (contextMatches iiifV3ImageContextString) contextValues then
        map (IIIFInfo IIIFV3) (iiifInfoDecoderWith "id" (strictOptionalObject "profile" v3InfoProfileDecoder))

    else if List.any (contextMatches iiifV2ImageContextString) contextValues then
        map (IIIFInfo IIIFV2) (iiifInfoDecoderWith "@id" (strictOptionalField "profile" v2InfoProfileDecoder))

    else if List.any isV1ImageContext contextValues then
        map (IIIFInfo IIIFV1) v1InfoDecoder

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
