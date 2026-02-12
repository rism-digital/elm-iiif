module IIIF.Presentation exposing
    ( IIIFManifest(..), IIIFCollection(..), IIIFCanvas(..), IIIFRange(..), IIIFResource(..)
    , Manifest, Canvas, Image, Range, RangeItem(..), Provider
    , ViewingDirection(..), ViewingHint(..), ViewingLayout(..), Behavior(..)
    , MediaFormats(..), ResourceTypes(..), ServiceTypes(..), ImageType(..)
    , HomePage, Logo, ServiceObject, SeeAlso, Thumbnail, Service, RequiredStatement, PaintedMedia
    , Collection, CollectionItem(..)
    , toManifest, withManifest, toLabel, toSummary, toMetadata, toHomepage, toLogo, toProvider, toRanges
    , toViewingDirection, toRequiredStatement, toCanvases, manifestViewingLayout
    , primaryImageUriForCanvas, isPagedLayout, canvasAspect, canvasLabel
    , stringToViewingDirection, stringToViewingHint, stringToBehavior, stringToServiceType
    , mediaFormatFromString, resourceTypeFromString
    )

{-| IIIF Presentation API types and convenience helpers.

This module normalizes V2 and V3 data types into a shared state. The different versioned response types
will hold a decoded identifier for the specific IIIF version.


# Versioned Responses

@docs IIIFManifest, IIIFCollection, IIIFCanvas, IIIFRange, IIIFResource


# Manifest Structure

@docs Manifest, Canvas, Image, Range, RangeItem, Provider
@docs ViewingDirection, ViewingHint, ViewingLayout, Behavior
@docs MediaFormats, ResourceTypes, ServiceTypes, ImageType
@docs HomePage, Logo, ServiceObject, SeeAlso, Thumbnail, Service, RequiredStatement, PaintedMedia


# Collection Structure

@docs Collection, CollectionItem


# Helpers

@docs toManifest, withManifest, toLabel, toSummary, toMetadata, toHomepage, toLogo, toProvider, toRanges
@docs toViewingDirection, toRequiredStatement, toCanvases, manifestViewingLayout
@docs primaryImageUriForCanvas, isPagedLayout, canvasAspect, canvasLabel
@docs stringToViewingDirection, stringToViewingHint, stringToBehavior, stringToServiceType
@docs mediaFormatFromString, resourceTypeFromString

-}

import IIIF.Image exposing (ImageUri)
import IIIF.Language exposing (LabelValue, Language(..), LanguageMap, extractLabelFromLanguageMap)
import IIIF.Version exposing (IIIFVersion)


{-| The core manifest record. This record type normalizes the differences between
v2 and v3 manifests, making them both fit the same structure. The differences are
normalized in the decoding process.
-}
type alias Manifest =
    { id : String
    , label : LanguageMap
    , metadata : List LabelValue
    , viewingDirection : ViewingDirection
    , summary : Maybe LanguageMap
    , viewingLayout : ViewingLayout
    , canvases : List Canvas
    , ranges : Maybe (List Range)
    , homepage : Maybe (List HomePage)
    , logo : Maybe Image
    , provider : Maybe (List Provider)
    , thumbnail : Maybe Image
    , requiredStatement : Maybe RequiredStatement
    }


{-| The core collection record (v2 and v3).
-}
type alias Collection =
    { id : String
    , label : LanguageMap
    , summary : Maybe LanguageMap
    , items : List CollectionItem
    }


{-| A version-tagged IIIF manifest wrapper.
-}
type IIIFManifest
    = IIIFManifest IIIFVersion Manifest


{-| A version-tagged IIIF collection wrapper.
-}
type IIIFCollection
    = IIIFCollection IIIFVersion Collection


{-| A version-tagged IIIF canvas wrapper.
-}
type IIIFCanvas
    = IIIFCanvas IIIFVersion Canvas


{-| A version-tagged IIIF range wrapper.
-}
type IIIFRange
    = IIIFRange IIIFVersion Range


{-| A collection item (either a collection or manifest). This is used to represent
the two possible content items in a IIIF Collection.
-}
type CollectionItem
    = NestedCollection Collection
    | ManifestItem Manifest


{-| A top-level IIIF resource. Useful for when you are requesting
a IIIF resource but you don't know whether you will get a manifest
or collection as a result.
-}
type IIIFResource
    = ResourceManifest IIIFManifest
    | ResourceCollection IIIFCollection
    | ResourceCanvas IIIFCanvas
    | ResourceRange IIIFRange


{-| A linked IIIF image reference.
-}
type alias Image =
    { id : ImageUri
    , label : Maybe LanguageMap
    , imageType : ImageType
    , service : List ServiceTypes
    }


{-| A IIIF canvas record (v2 and v3).
-}
type alias Canvas =
    { id : String
    , label : Maybe LanguageMap
    , width : Maybe Int
    , height : Maybe Int
    , images : List Image
    }


{-| A IIIF range record.
-}
type alias Range =
    { id : String
    , label : LanguageMap
    , items : List RangeItem
    , metadata : List LabelValue
    }


{-| A IIIF provider record.
-}
type alias Provider =
    { id : String
    , label : LanguageMap
    , type_ : ResourceTypes
    , homepage : Maybe (List HomePage)
    , logo : Maybe (List Logo)
    , seeAlso : Maybe (List SeeAlso)
    }


{-| A range item (range or canvas).
-}
type RangeItem
    = RangeCanvas String
    | RangeRange Range


{-| Viewing direction for a manifest.
-}
type ViewingDirection
    = LeftToRight
    | RightToLeft
    | TopToBottom
    | BottomToTop


{-| Manifest Behavior hints (IIIF v3; See ViewingHint for v2).
-}
type Behavior
    = AutoAdvanceBehavior
    | NoAutoAdvanceBehavior
    | RepeatBehavior
    | NoRepeatBehavior
    | UnorderedBehavior
    | IndividualsBehavior
    | ContinuousBehavior
    | PagedBehavior
    | FacingPagesBehavior
    | NonPagedBehavior
    | MultiPartBehavior
    | TogetherBehavior
    | SequenceBehavior
    | ThumbnailNavBehavior
    | NoNavBehavior
    | HiddenBehavior


{-| Viewing hint values (IIIF v2; See Behavior for v3).
-}
type ViewingHint
    = PagedHint
    | IndividualsHint
    | ContinuousHint
    | MultiPartHint
    | NonPagedHint
    | TopHint
    | FacingPagesHint


{-| Viewing layout values. Uses different types for the V2 and V3 APIs.
-}
type ViewingLayout
    = LayoutV2 ViewingHint
    | LayoutV3 (List Behavior)


{-| Supported media format identifiers.
-}
type MediaFormats
    = ImageJpeg
    | OtherFormat String


{-| Resource type identifiers.
-}
type ResourceTypes
    = Video
    | OtherResource String


{-| Service type identifiers.
-}
type ServiceTypes
    = ImageService1
    | ImageService2
    | ImageService3
    | SearchService1
    | AutoCompleteService1
    | AuthTokenService1
    | AuthLogoutService1
    | UnknownService


{-| Image type values for a canvas image.
-}
type ImageType
    = PrimaryImage
    | ChoiceImage


{-| A minimal painted media reference (used for canvas annotation bodies).
-}
type alias PaintedMedia =
    { id : String
    }


{-| A linked homepage record.
-}
type alias HomePage =
    { id : String
    , label : LanguageMap
    , format : MediaFormats
    , type_ : ResourceTypes
    }


{-| A logo image record.
-}
type alias Logo =
    { id : String
    , label : LanguageMap
    , format : MediaFormats
    , type_ : ResourceTypes
    , width : Int
    , height : Int
    , service : Maybe (List ServiceObject)
    }


{-| A service record attached to another resource.
-}
type alias ServiceObject =
    { id : String
    , serviceType : ServiceTypes
    }


{-| A linked `seeAlso` record.
-}
type alias SeeAlso =
    { id : String
    , label : LanguageMap
    , format : MediaFormats
    , type_ : ResourceTypes
    }


{-| A thumbnail record.
-}
type alias Thumbnail =
    { id : String
    , label : LanguageMap
    , format : MediaFormats
    , type_ : ResourceTypes
    , mediaServices : List Service
    }


{-| A generic service record.
-}
type alias Service =
    { id : String
    , type_ : ServiceTypes
    , profile : String
    }


{-| Required statement metadata.
-}
type alias RequiredStatement =
    { label : LanguageMap
    , value : LanguageMap
    }


{-| Unwrap a version-tagged manifest.
-}
toManifest : IIIFManifest -> Manifest
toManifest (IIIFManifest _ iiifManifest) =
    iiifManifest


{-| Apply an accessor to the underlying manifest.
-}
withManifest : (Manifest -> a) -> IIIFManifest -> a
withManifest accessor manifest =
    accessor (toManifest manifest)


{-| Read the manifest label.
-}
toLabel : IIIFManifest -> LanguageMap
toLabel =
    withManifest .label


{-| Read the manifest summary.
-}
toSummary : IIIFManifest -> Maybe LanguageMap
toSummary =
    withManifest .summary


{-| Read the manifest metadata list.
-}
toMetadata : IIIFManifest -> List LabelValue
toMetadata =
    withManifest .metadata


{-| Read the manifest homepage links.
-}
toHomepage : IIIFManifest -> Maybe (List HomePage)
toHomepage =
    withManifest .homepage


{-| Read the manifest logo image.
-}
toLogo : IIIFManifest -> Maybe Image
toLogo =
    withManifest .logo


{-| Read the manifest provider list.
-}
toProvider : IIIFManifest -> Maybe (List Provider)
toProvider =
    withManifest .provider


{-| Read the manifest ranges (if present).
-}
toRanges : IIIFManifest -> Maybe (List Range)
toRanges =
    withManifest .ranges


{-| Read the manifest viewing direction.
-}
toViewingDirection : IIIFManifest -> ViewingDirection
toViewingDirection =
    withManifest .viewingDirection


{-| Read the manifest required statement (if present).
-}
toRequiredStatement : IIIFManifest -> Maybe RequiredStatement
toRequiredStatement =
    withManifest .requiredStatement


{-| Read the manifest canvases.
-}
toCanvases : IIIFManifest -> List Canvas
toCanvases =
    withManifest .canvases


{-| Read the manifest viewing layout.
-}
manifestViewingLayout : IIIFManifest -> ViewingLayout
manifestViewingLayout =
    withManifest .viewingLayout


{-| Pick the primary image URI for a canvas (if available).

On canvases with only one image, it will return the URI of the first; on canvases
with multiple images it will return the URI of the Primary image, or, if no images
are marked as the primary one, it will return the first in the list.

If the list of images on the canvas is empty, it will return Nothing.

-}
primaryImageUriForCanvas : Canvas -> Maybe ImageUri
primaryImageUriForCanvas canvas =
    choosePrimaryImage canvas.images
        |> Maybe.map .id


{-| Choose the Primary image to display from a list of candidates. If
no image is marked as primary, it will return the first.

If the list of images on a canvas is empty, it will return Nothing.

-}
choosePrimaryImage : List Image -> Maybe Image
choosePrimaryImage images =
    case List.filter (\img -> img.imageType == PrimaryImage) images of
        [] ->
            List.head images

        first :: _ ->
            Just first


{-| True when the layout represents a paged/spread view.
-}
isPagedLayout : ViewingLayout -> Bool
isPagedLayout layout =
    case layout of
        LayoutV2 hint ->
            case hint of
                PagedHint ->
                    True

                _ ->
                    False

        LayoutV3 behaviour ->
            List.member PagedBehavior behaviour


{-| Compute the aspect ratio for a canvas.

If (for whatever reason) the width and height of a canvas is not
available, or contains invalid values (e.g., "0") then this will
return 1.

-}
canvasAspect : Canvas -> Float
canvasAspect canvas =
    case ( canvas.width, canvas.height ) of
        ( Just w, Just h ) ->
            if w > 0 then
                toFloat h / toFloat w

            else
                1

        _ ->
            1


{-| Read a canvas label as a string (default language).
-}
canvasLabel : Canvas -> String
canvasLabel canvas =
    case canvas.label of
        Just langMap ->
            extractLabelFromLanguageMap Default langMap

        Nothing ->
            "Untitled"


{-| Convert a string to a viewing direction value.
-}
stringToViewingDirection : String -> ViewingDirection
stringToViewingDirection direction =
    case direction of
        "bottom-to-top" ->
            BottomToTop

        "left-to-right" ->
            LeftToRight

        "right-to-left" ->
            RightToLeft

        "top-to-bottom" ->
            TopToBottom

        _ ->
            LeftToRight


{-| Convert a string to a viewing hint value.
-}
stringToViewingHint : String -> ViewingHint
stringToViewingHint hint =
    case hint of
        "continuous" ->
            ContinuousHint

        "facing-pages" ->
            FacingPagesHint

        "individuals" ->
            IndividualsHint

        "multi-part" ->
            MultiPartHint

        "non-paged" ->
            NonPagedHint

        "paged" ->
            PagedHint

        "top" ->
            TopHint

        _ ->
            PagedHint


{-| Convert a string to a behavior value.
-}
stringToBehavior : String -> Behavior
stringToBehavior behavior =
    case behavior of
        "auto-advance" ->
            AutoAdvanceBehavior

        "continuous" ->
            ContinuousBehavior

        "facing-pages" ->
            FacingPagesBehavior

        "hidden" ->
            HiddenBehavior

        "individuals" ->
            IndividualsBehavior

        "multi-part" ->
            MultiPartBehavior

        "no-auto-advance" ->
            NoAutoAdvanceBehavior

        "no-nav" ->
            NoNavBehavior

        "no-repeat" ->
            NoRepeatBehavior

        "non-paged" ->
            NonPagedBehavior

        "paged" ->
            PagedBehavior

        "repeat" ->
            RepeatBehavior

        "sequence" ->
            SequenceBehavior

        "thumbnail-nav" ->
            ThumbnailNavBehavior

        "together" ->
            TogetherBehavior

        "unordered" ->
            UnorderedBehavior

        _ ->
            PagedBehavior


{-| Convert a string to a service type.
-}
stringToServiceType : String -> ServiceTypes
stringToServiceType val =
    case val of
        "AuthLogoutService1" ->
            AuthLogoutService1

        "AuthTokenService1" ->
            AuthTokenService1

        "AutoCompleteService1" ->
            AutoCompleteService1

        "ImageService1" ->
            ImageService1

        "ImageService2" ->
            ImageService2

        "ImageService3" ->
            ImageService3

        "SearchService1" ->
            SearchService1

        "http://iiif.io/api/image/2/context.json" ->
            ImageService2

        "http://iiif.io/api/image/3/context.json" ->
            ImageService3

        _ ->
            UnknownService


{-| Convert a string to a media format identifier.
-}
mediaFormatFromString : String -> MediaFormats
mediaFormatFromString value =
    case value of
        "image/jpeg" ->
            ImageJpeg

        _ ->
            OtherFormat value


{-| Convert a string to a resource type identifier.
-}
resourceTypeFromString : String -> ResourceTypes
resourceTypeFromString value =
    case value of
        "Video" ->
            Video

        _ ->
            OtherResource value
