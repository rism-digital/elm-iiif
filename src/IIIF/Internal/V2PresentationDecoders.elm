module IIIF.Internal.V2PresentationDecoders exposing (v2ResourceTypeDecoder, v2iiifManifestDecoder)

import IIIF.Image exposing (ImageUri)
import IIIF.Internal.Contexts exposing (contextMatches, iiifV2ImageContextString)
import IIIF.Internal.SharedDecoders exposing (convertImageIdToImageUri, convertStaticImageIdToImageUri, convertThumbnailImageIdToImageUri, formatDecoder, resourceTypeDecoder, thumbnailDecoder, viewingDirectionDecoder, viewingHintDecoder)
import IIIF.Internal.Utilities exposing (custom, hardcoded, optional, required, requiredAt)
import IIIF.Language exposing (Language(..), LanguageMap, LanguageValues(..), labelValueDecoder, v2LabelValueDecoder, v2LanguageMapLabelDecoder)
import IIIF.Presentation exposing (Canvas, Collection, CollectionItem(..), HomePage, IIIFCanvas(..), IIIFCollection(..), IIIFManifest(..), IIIFRange(..), IIIFResource(..), Image, ImageType(..), Manifest, MediaFormats(..), Range, RangeItem(..), RequiredStatement, ResourceTypes(..), ServiceTypes, ViewingDirection(..), ViewingHint(..), ViewingLayout(..), stringToServiceType)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode
    exposing
        ( Decoder
        , Value
        , andThen
        , at
        , fail
        , field
        , index
        , int
        , lazy
        , list
        , map
        , map2
        , maybe
        , oneOf
        , string
        , succeed
        , value
        )


v2iiifManifestDecoder : Decoder Manifest
v2iiifManifestDecoder =
    succeed Manifest
        |> required "@id" string
        |> required "label" v2LanguageMapLabelDecoder
        |> optional "metadata" (list v2LabelValueDecoder) []
        |> optional "viewingDirection" viewingDirectionDecoder LeftToRight
        |> optional "description" (maybe v2LanguageMapLabelDecoder) Nothing
        |> optional "viewingHint" viewingHintDecoder (LayoutV2 IndividualsHint)
        |> required "sequences" v2SequencesDecoder
        |> optional "structures" (maybe (list v2RangeDecoder)) Nothing
        -- 'rendering' is 'homepage' (v3) in the Manifest record.
        |> optional "rendering" (maybe v2HomePageListDecoder) Nothing
        |> optional "logo" (maybe v2ImageDecoder) Nothing
        -- v2 manifests do not have a provider section.
        |> hardcoded Nothing
        |> optional "thumbnail" (thumbnailDecoder v2ThumbnailImageDecoder) Nothing
        |> optional "attribution" (maybe v2RequiredStatement) Nothing


v2SequencesDecoder : Decoder (List Canvas)
v2SequencesDecoder =
    list v2CanvasDecoder
        |> at [ "canvases" ]
        |> index 0


v2CanvasDecoder : Decoder Canvas
v2CanvasDecoder =
    succeed Canvas
        |> required "@id" string
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> optional "width" (maybe int) Nothing
        |> optional "height" (maybe int) Nothing
        |> optional "images" v2AnnotationListDecoder []
        |> optional "thumbnail" (thumbnailDecoder v2ThumbnailImageDecoder) Nothing
        |> optional "viewingHint" (maybe viewingHintDecoder) Nothing


v2AnnotationListDecoder : Decoder (List Image)
v2AnnotationListDecoder =
    oneOf
        [ list (at [ "resource" ] v2ImageDecoder)
        , list (at [ "resource" ] v2ChoiceObjectDecoder)
            |> map unwrapDecoderLists
        ]


{-| Since we're decoding a list in the `images` block, and then
creating a list from both the 'default' and 'item' block for
the `oa:Choice` setup, we need to unwrap a the nested lists and
return just a single list of IIIFImage data.
-}
unwrapDecoderLists : List (List Image) -> List Image
unwrapDecoderLists =
    List.concat


v2ImageDecoder : Decoder Image
v2ImageDecoder =
    succeed Image
        |> custom v2ImageIdDecoder
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded PrimaryImage
        |> custom v2ImageServiceTypesDecoder
        |> custom v2ImageServiceObjectsDecoder


v2ImageDecoderVaryingType : ImageType -> Decoder Image
v2ImageDecoderVaryingType imgType =
    succeed Image
        |> custom v2ImageIdDecoder
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded imgType
        |> custom v2ImageServiceTypesDecoder
        |> custom v2ImageServiceObjectsDecoder


v2ThumbnailImageDecoder : Decoder Image
v2ThumbnailImageDecoder =
    succeed Image
        |> custom v2ThumbnailImageIdDecoder
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded PrimaryImage
        |> custom v2ImageServiceTypesDecoder
        |> custom v2ImageServiceObjectsDecoder


v2ChoiceObjectDecoder : Decoder (List Image)
v2ChoiceObjectDecoder =
    map2 (::)
        (at [ "default" ] v2ImageDecoder)
        (at [ "item" ] (list (v2ImageDecoderVaryingType ChoiceImage)))


v2ImageIdDecoder : Decoder ImageUri
v2ImageIdDecoder =
    oneOf
        [ v2ImageIdFromServiceDecoder
        , field "@id" string |> andThen convertStaticImageIdToImageUri
        ]


v2ImageServiceTypesDecoder : Decoder (List ServiceTypes)
v2ImageServiceTypesDecoder =
    oneOf
        [ field "service"
            (oneOf
                [ list (maybe v2ServiceTypeObjectDecoder) |> map (List.filterMap identity)
                , v2ServiceTypeObjectDecoder |> map List.singleton
                ]
            )
        , succeed []
        ]


v2ImageServiceObjectsDecoder : Decoder (List Value)
v2ImageServiceObjectsDecoder =
    oneOf
        [ field "service" (list value)
        , field "service" (value |> map List.singleton)
        , succeed []
        ]


v2ThumbnailImageIdDecoder : Decoder ImageUri
v2ThumbnailImageIdDecoder =
    oneOf
        [ v2ImageIdFromServiceDecoder
        , field "@id" string |> andThen convertThumbnailImageIdToImageUri
        ]


v2ImageIdFromServiceDecoder : Decoder ImageUri
v2ImageIdFromServiceDecoder =
    field "service"
        (oneOf
            [ list (maybe v2ImageServiceIdDecoder)
                |> map (List.filterMap identity >> List.head)
            , v2ImageServiceIdDecoder |> map Just
            ]
        )
        |> andThen
            (\maybeId ->
                case maybeId of
                    Just id ->
                        convertImageIdToImageUri id

                    Nothing ->
                        fail "No Image API service ID found"
            )


v2ImageServiceIdDecoder : Decoder String
v2ImageServiceIdDecoder =
    v2ServiceTypeObjectDecoder
        |> andThen
            (\serviceType ->
                if isImageService serviceType then
                    field "@id" string

                else
                    fail "Not an Image API service"
            )


v2ServiceTypeObjectDecoder : Decoder ServiceTypes
v2ServiceTypeObjectDecoder =
    oneOf
        [ field "@type" string |> map stringToServiceType
        , field "type" string |> map stringToServiceType
        , field "@context" string
            |> map
                (\context ->
                    if contextMatches iiifV2ImageContextString context then
                        stringToServiceType "ImageService2"

                    else
                        stringToServiceType context
                )
        ]


isImageService : ServiceTypes -> Bool
isImageService serviceType =
    List.member serviceType
        [ stringToServiceType "ImageService1"
        , stringToServiceType "ImageService2"
        , stringToServiceType "ImageService3"
        ]


v2RangeDecoder : Decoder Range
v2RangeDecoder =
    lazy (\_ -> v2RangeDecoderHelp)


v2RangeDecoderHelp : Decoder Range
v2RangeDecoderHelp =
    succeed Range
        |> required "@id" string
        |> optional "label" v2LanguageMapLabelDecoder defaultLabelMap
        |> custom v2RangeItemsDecoder
        |> optional "metadata" (list v2LabelValueDecoder) []


v2RangeItemsDecoder : Decoder (List RangeItem)
v2RangeItemsDecoder =
    map2
        (\subranges canvases ->
            List.map RangeRange subranges
                ++ List.map RangeCanvas canvases
        )
        (oneOf
            [ field "ranges" (list v2RangeDecoder)
            , succeed []
            ]
        )
        (oneOf
            [ field "canvases" (list string)
            , succeed []
            ]
        )


v2HomePageListDecoder : Decoder (List HomePage)
v2HomePageListDecoder =
    oneOf
        [ list v2HomePageDecoder
        , map List.singleton v2HomePageDecoder
        ]


v2HomePageDecoder : Decoder HomePage
v2HomePageDecoder =
    succeed HomePage
        |> required "@id" string
        |> optional "label" v2LanguageMapLabelDecoder defaultLabelMap
        |> optional "format" formatDecoder (OtherFormat "text/html")
        |> optional "@type" resourceTypeDecoder (OtherResource "Text")


defaultLabelMap : LanguageMap
defaultLabelMap =
    [ LanguageValues Default [ "Homepage" ] ]


v2iiifCollectionDecoder : Decoder Collection
v2iiifCollectionDecoder =
    succeed Collection
        |> required "@id" string
        |> required "label" v2LanguageMapLabelDecoder
        |> optional "description" (maybe v2LanguageMapLabelDecoder) Nothing
        |> custom v2CollectionItemsDecoder


{-| V2 collections can have items in either a "members" array OR separate
"collections" and "manifests" arrays. This decoder handles both cases.
-}
v2CollectionItemsDecoder : Decoder (List CollectionItem)
v2CollectionItemsDecoder =
    oneOf
        [ field "members" (list v2CollectionItemDecoder)
        , map2
            (\collections manifests -> collections ++ manifests)
            (oneOf
                [ field "collections" (list (map NestedCollection (lazy (\_ -> v2iiifCollectionDecoder))))
                , succeed []
                ]
            )
            (oneOf
                [ field "manifests" (list (map ManifestItem v2CollectionItemManifestDecoder))
                , succeed []
                ]
            )
        ]


v2CollectionItemDecoder : Decoder CollectionItem
v2CollectionItemDecoder =
    field "@type" string
        |> andThen v2CollectionItemFromType


v2CollectionItemFromType : String -> Decoder CollectionItem
v2CollectionItemFromType itemType =
    case itemType of
        "sc:Collection" ->
            map NestedCollection (lazy (\_ -> v2iiifCollectionDecoder))

        "sc:Manifest" ->
            map ManifestItem v2CollectionItemManifestDecoder

        _ ->
            fail ("Unknown collection item type: " ++ itemType)


v2RequiredStatement : Decoder RequiredStatement
v2RequiredStatement =
    oneOf
        [ string
            |> map
                (\value ->
                    { label = [ LanguageValues Default [ "Attribution" ] ]
                    , value = [ LanguageValues Default [ value ] ]
                    }
                )
        , labelValueDecoder
        , v2LabelValueDecoder
        ]


{-| Minimal manifest decoder for collection items - they contain only
id, label, description, thumbnail - not full canvas data
-}
v2CollectionItemManifestDecoder : Decoder Manifest
v2CollectionItemManifestDecoder =
    succeed Manifest
        |> required "@id" string
        |> required "label" v2LanguageMapLabelDecoder
        |> hardcoded []
        |> hardcoded LeftToRight
        |> optional "description" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded (LayoutV2 IndividualsHint)
        |> hardcoded []
        |> hardcoded Nothing
        |> optional "rendering" (maybe v2HomePageListDecoder) Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> optional "thumbnail" (thumbnailDecoder v2ThumbnailImageDecoder) Nothing
        |> hardcoded Nothing


v2ResourceTypeDecoder : Decoder IIIFResource
v2ResourceTypeDecoder =
    field "@type" string
        |> andThen v2ResourceFromType


v2ResourceFromType : String -> Decoder IIIFResource
v2ResourceFromType resourceType =
    case resourceType of
        "sc:Canvas" ->
            map (ResourceCanvas << IIIFCanvas IIIFV2) v2CanvasDecoder

        "sc:Collection" ->
            map (ResourceCollection << IIIFCollection IIIFV2) v2iiifCollectionDecoder

        "sc:Manifest" ->
            map (ResourceManifest << IIIFManifest IIIFV2) v2iiifManifestDecoder

        "sc:Range" ->
            map (ResourceRange << IIIFRange IIIFV2) v2RangeDecoder

        _ ->
            fail ("Unknown IIIF v2 resource type: " ++ resourceType)
