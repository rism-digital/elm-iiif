module IIIF.Internal.V2PresentationDecoders exposing (v2ResourceTypeDecoder, v2iiifManifestDecoder)

import IIIF.Internal.SharedDecoders exposing (convertImageIdToImageUri, formatDecoder, resourceTypeDecoder, viewingDirectionDecoder, viewingHintDecoder)
import IIIF.Internal.Utilities exposing (custom, hardcoded, optional, required, requiredAt)
import IIIF.Language exposing (Language(..), LanguageMap, LanguageValues(..), labelValueDecoder, v2LabelValueDecoder, v2LanguageMapLabelDecoder)
import IIIF.Presentation exposing (Canvas, Collection, CollectionItem(..), HomePage, IIIFCanvas(..), IIIFCollection(..), IIIFManifest(..), IIIFRange(..), IIIFResource(..), Image, ImageType(..), Manifest, MediaFormats(..), Range, RangeItem(..), RequiredStatement, ResourceTypes(..), ServiceTypes, ViewingDirection(..), ViewingHint(..), ViewingLayout(..), stringToServiceType)
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode exposing (Decoder, andThen, at, int, lazy, list, map, map2, maybe, oneOf, string, succeed)


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
        |> optional "thumbnail" (maybe v2ImageDecoder) Nothing
        |> optional "attribution" (maybe v2RequiredStatement) Nothing


v2SequencesDecoder : Decoder (List Canvas)
v2SequencesDecoder =
    list v2CanvasDecoder
        |> at [ "canvases" ]
        |> Decode.index 0


v2CanvasDecoder : Decoder Canvas
v2CanvasDecoder =
    succeed Canvas
        |> required "@id" string
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> optional "width" (maybe int) Nothing
        |> optional "height" (maybe int) Nothing
        |> optional "images" v2AnnotationListDecoder []
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
        |> requiredAt [ "service", "@id" ] (string |> andThen convertImageIdToImageUri)
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded PrimaryImage
        |> requiredAt [ "service", "@context" ] (string |> map v2ServiceTypeDecoder)


v2ImageDecoderVaryingType : ImageType -> Decoder Image
v2ImageDecoderVaryingType imgType =
    succeed Image
        |> required "@id"
            (string
                |> andThen convertImageIdToImageUri
            )
        |> optional "label" (maybe v2LanguageMapLabelDecoder) Nothing
        |> hardcoded imgType
        |> requiredAt [ "service", "@context" ] (string |> map v2ServiceTypeDecoder)


v2ChoiceObjectDecoder : Decoder (List Image)
v2ChoiceObjectDecoder =
    map2 (::)
        (at [ "default" ] v2ImageDecoder)
        (at [ "item" ] (list (v2ImageDecoderVaryingType ChoiceImage)))


v2ServiceTypeDecoder : String -> List ServiceTypes
v2ServiceTypeDecoder stype =
    [ stringToServiceType stype ]


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
            [ Decode.field "ranges" (list v2RangeDecoder)
            , succeed []
            ]
        )
        (oneOf
            [ Decode.field "canvases" (list string)
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
        [ Decode.field "members" (list v2CollectionItemDecoder)
        , map2
            (\collections manifests -> collections ++ manifests)
            (oneOf
                [ Decode.field "collections" (list (map NestedCollection (lazy (\_ -> v2iiifCollectionDecoder))))
                , succeed []
                ]
            )
            (oneOf
                [ Decode.field "manifests" (list (map ManifestItem v2CollectionItemManifestDecoder))
                , succeed []
                ]
            )
        ]


v2CollectionItemDecoder : Decoder CollectionItem
v2CollectionItemDecoder =
    Decode.field "@type" string
        |> andThen v2CollectionItemFromType


v2CollectionItemFromType : String -> Decoder CollectionItem
v2CollectionItemFromType itemType =
    case itemType of
        "sc:Collection" ->
            map NestedCollection (lazy (\_ -> v2iiifCollectionDecoder))

        "sc:Manifest" ->
            map ManifestItem v2CollectionItemManifestDecoder

        _ ->
            Decode.fail ("Unknown collection item type: " ++ itemType)


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
        |> optional "thumbnail" (maybe v2ImageDecoder) Nothing
        |> hardcoded Nothing


v2ResourceTypeDecoder : Decoder IIIFResource
v2ResourceTypeDecoder =
    Decode.field "@type" string
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
            Decode.fail ("Unknown IIIF v2 resource type: " ++ resourceType)
