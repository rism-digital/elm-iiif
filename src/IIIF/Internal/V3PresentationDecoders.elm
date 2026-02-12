module IIIF.Internal.V3PresentationDecoders exposing (v3ResourceTypeDecoder, v3iiifManifestDecoder)

import IIIF.Image exposing (ImageUri, imageUriToInfoUri, parseImageAddress)
import IIIF.Internal.SharedDecoders exposing (behaviourDecoder, convertImageIdToImageUri, formatDecoder, resourceTypeDecoder, serviceTypeDecoder, viewingDirectionDecoder)
import IIIF.Internal.Utilities exposing (custom, hardcoded, optional, required)
import IIIF.Language exposing (Language(..), LanguageMap, LanguageValues(..), labelValueDecoder, languageMapLabelDecoder, stringToLanguageMapLabelDecoder)
import IIIF.Presentation exposing (Behavior(..), Canvas, Collection, CollectionItem(..), HomePage, IIIFCanvas(..), IIIFCollection(..), IIIFManifest(..), IIIFRange(..), IIIFResource(..), Image, ImageType(..), Logo, Manifest, MediaFormats(..), Provider, Range, RangeItem(..), ResourceTypes(..), SeeAlso, ServiceObject, ServiceTypes(..), ViewingDirection(..), ViewingLayout(..))
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder, andThen, at, fail, field, index, int, list, map, map3, maybe, oneOf, string, succeed)


v3iiifManifestDecoder : Decoder Manifest
v3iiifManifestDecoder =
    succeed Manifest
        |> required "id" string
        |> required "label" v3LabelDecoder
        |> optional "metadata" (list labelValueDecoder) []
        |> optional "viewingDirection" viewingDirectionDecoder LeftToRight
        |> optional "summary" (maybe languageMapLabelDecoder) Nothing
        |> optional "behavior" behaviourDecoder (LayoutV3 [ IndividualsBehavior ])
        |> required "items" (list v3CanvasDecoder)
        |> optional "structures" (maybe (list v3RangeDecoder)) Nothing
        |> optional "homepage" (maybe (list v3HomePageDecoder)) Nothing
        |> optional "logo" (maybe (v3ImageDecoder PrimaryImage)) Nothing
        |> optional "provider" (maybe (list v3ProviderDecoder)) Nothing
        |> optional "thumbnail" (maybe (v3ImageDecoder PrimaryImage)) Nothing
        |> optional "requiredStatement" (maybe labelValueDecoder) Nothing


v3iiifCollectionDecoder : Decoder Collection
v3iiifCollectionDecoder =
    succeed Collection
        |> required "id" string
        |> required "label" v3LabelDecoder
        |> optional "summary" (maybe languageMapLabelDecoder) Nothing
        |> custom v3CollectionItemsDecoder


v3CollectionItemsDecoder : Decoder (List CollectionItem)
v3CollectionItemsDecoder =
    map3
        (\items collections manifests ->
            if List.isEmpty items then
                collections ++ manifests

            else
                items ++ collections ++ manifests
        )
        (oneOf
            [ field "items" (list v3CollectionItemDecoder)
            , succeed []
            ]
        )
        (oneOf
            [ field "collections" (list (map NestedCollection (Decode.lazy (\_ -> v3iiifCollectionDecoder))))
            , succeed []
            ]
        )
        (oneOf
            [ field "manifests" (list (map ManifestItem v3CollectionItemManifestDecoder))
            , succeed []
            ]
        )


v3CollectionItemDecoder : Decoder CollectionItem
v3CollectionItemDecoder =
    field "type" string
        |> andThen v3CollectionItemFromType


v3CollectionItemFromType : String -> Decoder CollectionItem
v3CollectionItemFromType itemType =
    case itemType of
        "Collection" ->
            map NestedCollection (Decode.lazy (\_ -> v3iiifCollectionDecoder))

        "Manifest" ->
            map ManifestItem v3CollectionItemManifestDecoder

        _ ->
            fail ("Unknown collection item type: " ++ itemType)


{-| Minimal manifest decoder for collection items - they contain only
id, label, summary, thumbnail - not full canvas data
-}
v3CollectionItemManifestDecoder : Decoder Manifest
v3CollectionItemManifestDecoder =
    succeed Manifest
        |> required "id" string
        |> required "label" v3LabelDecoder
        |> hardcoded []
        |> hardcoded LeftToRight
        |> optional "summary" (maybe languageMapLabelDecoder) Nothing
        |> hardcoded (LayoutV3 [ IndividualsBehavior ])
        |> hardcoded []
        |> hardcoded Nothing
        |> optional "homepage" (maybe (list v3HomePageDecoder)) Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> optional "thumbnail" (maybe (v3ImageDecoder PrimaryImage)) Nothing
        |> hardcoded Nothing


v3CanvasDecoder : Decoder Canvas
v3CanvasDecoder =
    succeed Canvas
        |> required "id" string
        |> optional "label" (maybe languageMapLabelDecoder) Nothing
        |> required "width" (maybe int)
        |> required "height" (maybe int)
        |> required "items" (index 0 v3AnnotationPageDecoder)


v3AnnotationPageDecoder : Decoder (List Image)
v3AnnotationPageDecoder =
    index 0 v3AnnotationBodyDecoder
        |> at [ "items" ]


v3AnnotationBodyDecoder : Decoder (List Image)
v3AnnotationBodyDecoder =
    at [ "body", "type" ] string
        |> andThen v3AnnotationChoiceTypeDecoder


v3AnnotationChoiceTypeDecoder : String -> Decoder (List Image)
v3AnnotationChoiceTypeDecoder annotType =
    if annotType == "Choice" then
        v3ChoiceBodyDecoder

    else
        v3ImageBodyDecoder


v3ImageBodyDecoder : Decoder (List Image)
v3ImageBodyDecoder =
    at [ "body", "id" ] string
        |> andThen v3AnnotationImageDecoder


v3AnnotationImageDecoder : String -> Decoder (List Image)
v3AnnotationImageDecoder imageUrl =
    case parseImageAddress imageUrl of
        Just _ ->
            field "body" (v3ImageDecoder PrimaryImage)
                |> map List.singleton

        Nothing ->
            fail "Could not decode image url"


v3ImageDecoder : ImageType -> Decoder Image
v3ImageDecoder imageType =
    succeed Image
        |> custom v3ImageIdFromServiceDecoder
        |> optional "label" (maybe v3LabelDecoder) Nothing
        |> hardcoded imageType
        |> optional "service" v3ServiceTypeListDecoder []


v3ChoiceBodyDecoder : Decoder (List Image)
v3ChoiceBodyDecoder =
    v3ImageDecoder ChoiceImage
        |> list
        |> at [ "body", "items" ]


v3LabelDecoder : Decoder LanguageMap
v3LabelDecoder =
    oneOf
        [ languageMapLabelDecoder
        , stringToLanguageMapLabelDecoder
        ]


v3ServiceTypeDecoder : Decoder ServiceTypes
v3ServiceTypeDecoder =
    oneOf
        [ field "@type" string
        , field "type" string
        ]
        |> andThen serviceTypeDecoder


v3ServiceTypeListDecoder : Decoder (List ServiceTypes)
v3ServiceTypeListDecoder =
    oneOf
        [ list v3ServiceTypeDecoder
        , v3ServiceTypeDecoder |> map List.singleton
        ]


v3RangeDecoder : Decoder Range
v3RangeDecoder =
    succeed Range
        |> required "id" string
        |> optional "label" languageMapLabelDecoder defaultLabelMap
        |> optional "items" (list v3RangeItemDecoder) []
        |> optional "metadata" (list labelValueDecoder) []


v3RangeItemDecoder : Decoder RangeItem
v3RangeItemDecoder =
    field "type" string
        |> andThen v3RangeItemFromType


v3RangeItemFromType : String -> Decoder RangeItem
v3RangeItemFromType itemType =
    case itemType of
        "Canvas" ->
            field "id" string
                |> map RangeCanvas

        "Range" ->
            map RangeRange v3RangeDecoder

        _ ->
            fail "Unsupported range item type"


v3HomePageDecoder : Decoder HomePage
v3HomePageDecoder =
    succeed HomePage
        |> required "id" string
        |> optional "label" languageMapLabelDecoder defaultLabelMap
        |> optional "format" formatDecoder (OtherFormat "text/html")
        |> optional "type" resourceTypeDecoder (OtherResource "Text")


defaultLabelMap : LanguageMap
defaultLabelMap =
    [ LanguageValues Default [ "Homepage" ] ]


v3ProviderDecoder : Decoder Provider
v3ProviderDecoder =
    succeed Provider
        |> required "id" string
        |> required "label" languageMapLabelDecoder
        |> required "type" resourceTypeDecoder
        |> optional "homepage" (maybe (list v3HomePageDecoder)) Nothing
        |> optional "logo" (maybe (list v3LogoDecoder)) Nothing
        |> optional "seeAlso" (maybe (list v3SeeAlsoDecoder)) Nothing


v3LogoDecoder : Decoder Logo
v3LogoDecoder =
    succeed Logo
        |> required "id" string
        |> optional "label" languageMapLabelDecoder defaultLogoLabelMap
        |> optional "format" formatDecoder (OtherFormat "image/png")
        |> optional "type" resourceTypeDecoder (OtherResource "Image")
        |> optional "width" int 0
        |> optional "height" int 0
        |> optional "service" (maybe v3ServiceObjectListDecoder) Nothing


v3SeeAlsoDecoder : Decoder SeeAlso
v3SeeAlsoDecoder =
    succeed SeeAlso
        |> required "id" string
        |> required "label" v3LabelDecoder
        |> required "format" formatDecoder
        |> required "type_" resourceTypeDecoder


defaultLogoLabelMap : LanguageMap
defaultLogoLabelMap =
    [ LanguageValues Default [ "Logo" ] ]


v3ServiceObjectDecoder : Decoder ServiceObject
v3ServiceObjectDecoder =
    succeed ServiceObject
        |> custom (oneOf [ field "id" string, field "@id" string ])
        |> custom v3ServiceTypeDecoder


v3ServiceObjectListDecoder : Decoder (List ServiceObject)
v3ServiceObjectListDecoder =
    oneOf
        [ list v3ServiceObjectDecoder
        , v3ServiceObjectDecoder |> map List.singleton
        ]


selectServiceId : List ServiceObject -> Maybe String
selectServiceId services =
    let
        imageService3Id =
            List.filter (\s -> s.serviceType == ImageService3) services
                |> List.head
                |> Maybe.map .id
    in
    case imageService3Id of
        Just id ->
            Just id

        Nothing ->
            List.head services
                |> Maybe.map .id


v3ImageIdFromServiceDecoder : Decoder ImageUri
v3ImageIdFromServiceDecoder =
    field "service" v3ServiceObjectListDecoder
        |> andThen
            (\services ->
                case selectServiceId services of
                    Just id ->
                        convertImageIdToImageUri id
                            |> map imageUriToInfoUri

                    Nothing ->
                        fail "No valid service ID found in service array"
            )


v3ResourceTypeDecoder : Decoder IIIFResource
v3ResourceTypeDecoder =
    field "type" string
        |> andThen v3ResourceFromType


v3ResourceFromType : String -> Decoder IIIFResource
v3ResourceFromType resourceType =
    case resourceType of
        "Canvas" ->
            map (ResourceCanvas << IIIFCanvas IIIFV3) v3CanvasDecoder

        "Collection" ->
            map (ResourceCollection << IIIFCollection IIIFV3) v3iiifCollectionDecoder

        "Manifest" ->
            map (ResourceManifest << IIIFManifest IIIFV3) v3iiifManifestDecoder

        "Range" ->
            map (ResourceRange << IIIFRange IIIFV3) v3RangeDecoder

        _ ->
            fail ("Unknown IIIF v3 resource type: " ++ resourceType)
