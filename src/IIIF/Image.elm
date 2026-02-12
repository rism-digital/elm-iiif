module IIIF.Image exposing
    ( ImageUri(..), ImageServerParameters, ImageRequestParameters, ImageRegion(..), ImageSize(..)
    , ImageRotation(..), ImageQuality(..), ImageFormat(..)
    , thumbnailUrlFromInfo
    , createImageAddress, infoUriToImageUri, imageUriToInfoUri, imageUriFromComponents, infoUriFromComponents
    , setImageUriSize, setImageUriRegion, imageServerToImageRequest
    , createInfoUri, createImageUri, createRegionComponent
    , createSizeComponent, createRotationComponent, createQualityComponent, createFormatComponent
    , parseImageUrl, parseImageAddress
    , qualityParser, formatParser, rotationParser, normalRotationParser, mirroredRotationParser
    , imageSizeParser, maxSizeParser, widthOnlySizeParser, exactWidthOnlySizeParser
    , heightOnlySizeParser, exactHeightOnlySizeParser, percentSizeParser, exactPercentSizeParser
    , whSizeParser, widthAndHeightSizeParser, exactWidthAndHeightSizeParser
    , scaledWidthAndHeightSizeParser, exactScaledWidthAndHeightSizeParser
    , imageRegionParser, fullSquareRegionParser, sizeRegionParser, pctSizeRegionParser
    )

{-| Types and utilities for IIIF Image API URLs.

This module focuses on parsing and constructing image and info.json URLs, and converting between the two
types.

Most of the Image API parameters are provided as types. The various helpers then take care of writing the URLs
in the correct format; for example, an ImageUri with a size component of `WidthOnlySize 180` will generate a URL
component of `180,`, while one with `ExactWidthAndHeightSize (180, 250)` will generate `^180,250`.


# Definitions

@docs ImageUri, ImageServerParameters, ImageRequestParameters, ImageRegion, ImageSize
@docs ImageRotation, ImageQuality, ImageFormat


# Helpers

@docs thumbnailUrlFromInfo


# Creating Image API Urls

@docs createImageAddress, infoUriToImageUri, imageUriToInfoUri, imageUriFromComponents, infoUriFromComponents
@docs setImageUriSize, setImageUriRegion, imageServerToImageRequest
@docs createInfoUri, createImageUri, createRegionComponent
@docs createSizeComponent, createRotationComponent, createQualityComponent, createFormatComponent


# Parsing Image API Urls

@docs parseImageUrl, parseImageAddress
@docs qualityParser, formatParser, rotationParser, normalRotationParser, mirroredRotationParser
@docs imageSizeParser, maxSizeParser, widthOnlySizeParser, exactWidthOnlySizeParser
@docs heightOnlySizeParser, exactHeightOnlySizeParser, percentSizeParser, exactPercentSizeParser
@docs whSizeParser, widthAndHeightSizeParser, exactWidthAndHeightSizeParser
@docs scaledWidthAndHeightSizeParser, exactScaledWidthAndHeightSizeParser
@docs imageRegionParser, fullSquareRegionParser, sizeRegionParser, pctSizeRegionParser

-}

import IIIF.Internal.Utilities exposing (applyMaybe, cartesianProduct, oneOfMaybes, remove)
import Url exposing (Protocol(..), Url)


{-| A parsed IIIF Image URI. The InfoUri is used to construct URLs to the 'info.json' document,
while the ImageUri is used to construct Image API calls for regions, sizes, etc.

Example:

    imageUriToInfoUri (InfoUri server)

-}
type ImageUri
    = InfoUri ImageServerParameters
    | ImageUri ImageRequestParameters


{-| Parameters for the base image server URI (info.json).
-}
type alias ImageServerParameters =
    { host : String
    , prefix : String
    }


{-| Parameters for a full IIIF image request URI.

Example:

    { host = "https://example.org"
    , prefix = "iiif/2/abc123"
    , region = FullRegion
    , size = MaxSize
    , rotation = NormalRotation 0
    , quality = DefaultQuality
    , format = JpegFormat
    }

-}
type alias ImageRequestParameters =
    { host : String
    , prefix : String
    , region : ImageRegion
    , size : ImageSize
    , rotation : ImageRotation
    , quality : ImageQuality
    , format : ImageFormat
    }


{-| The region portion of an image request.

Example:

    SquareRegion

-}
type ImageRegion
    = FullRegion
    | SquareRegion
    | SizeRegion { x : Int, y : Int, w : Int, h : Int }
    | PctSizeRegion { x : Float, y : Float, w : Float, h : Float }


{-| The parameters allowed, in order, are

    <max>,
    <^max>,
    <w,>,
    <^w,>,
    <,h>,
    <^,h>,
    <pct:n>,
    <^pct:n>,
    <w,h>,
    <^w,h>,
    <!w,h>,
    <^!w,h>

For IIIF v2 the keyword <full> will be mapped to the <max> type. Since the
'full' value was marked as deprecated in IIIF 2.1, and the <max> type was available
then, any conversions back to IIIF v2 image URI will use the "max" string.

-}
type ImageSize
    = MaxSize
    | ExactMaxSize
    | WidthOnlySize Int
    | ExactWidthOnlySize Int
    | HeightOnlySize Int
    | ExactHeightOnlySize Int
    | PercentSize Float
    | ExactPercentSize Float
    | WidthAndHeightSize ( Int, Int )
    | ExactWidthAndHeightSize ( Int, Int )
    | ScaledWidthAndHeightSize ( Int, Int )
    | ExactScaledWidthAndHeightSize ( Int, Int )


{-| The rotation portion of an image request.
-}
type ImageRotation
    = NormalRotation Float
    | MirroredRotation Float


{-| The quality portion of an image request.
-}
type ImageQuality
    = ColorQuality
    | GrayQuality
    | BiTonalQuality
    | DefaultQuality
    | NativeQuality


{-| The format portion of an image request.
-}
type ImageFormat
    = JpegFormat
    | TiffFormat
    | PngFormat
    | Jp2Format
    | GifFormat
    | PdfFormat
    | WebpFormat


{-| Render a IIIFImageUri as a string URL.
-}
createImageAddress : ImageUri -> String
createImageAddress iiifUri =
    case iiifUri of
        InfoUri params ->
            createInfoUri params

        ImageUri params ->
            createImageUri params


{-| Parse a IIIF Image URL string into a IIIFImageUri record.
-}
parseImageAddress : String -> Maybe ImageUri
parseImageAddress fullAddress =
    Url.fromString fullAddress
        |> Maybe.andThen parseImageUrl


{-| Convert an URL for info.json to a image request URI, using
a default set of arguments.
-}
infoUriToImageUri : ImageUri -> ImageUri
infoUriToImageUri inp =
    case inp of
        InfoUri params ->
            imageServerToImageRequest params
                |> ImageUri

        ImageUri _ ->
            inp


{-| Convert a full image request URI to its info.json URI.
-}
imageUriToInfoUri : ImageUri -> ImageUri
imageUriToInfoUri inp =
    case inp of
        InfoUri _ ->
            inp

        ImageUri params ->
            InfoUri { host = params.host, prefix = params.prefix }


{-| Set the size component of an image URI. If an InfoUri is provided
it will convert it to an ImageUri and then apply the size setting.
-}
setImageUriSize : ImageSize -> ImageUri -> ImageUri
setImageUriSize size uri =
    let
        normParams =
            case uri of
                InfoUri p ->
                    imageServerToImageRequest p

                ImageUri p ->
                    p
    in
    ImageUri { normParams | size = size }


{-| Set the region component of an IIIFImageUri. If a InfoUri is provided
it is converted to a ImageUri and then the region is set.
-}
setImageUriRegion : ImageRegion -> ImageUri -> ImageUri
setImageUriRegion region uri =
    let
        normParams =
            case uri of
                InfoUri p ->
                    imageServerToImageRequest p

                ImageUri p ->
                    p
    in
    ImageUri { normParams | region = region }


{-| Expand a base image server URI into default image request parameters.
-}
imageServerToImageRequest : ImageServerParameters -> ImageRequestParameters
imageServerToImageRequest { host, prefix } =
    { host = host
    , prefix = prefix
    , region = FullRegion
    , size = MaxSize
    , rotation = NormalRotation 0
    , quality = DefaultQuality
    , format = JpegFormat
    }


{-| Render a base image server URI as an `info.json` URL.
-}
createInfoUri : ImageServerParameters -> String
createInfoUri params =
    params.host ++ params.prefix ++ "/info.json"


{-| Render a full image request URI as a URL string.
-}
createImageUri : ImageRequestParameters -> String
createImageUri params =
    String.join "/"
        [ params.host ++ params.prefix
        , createRegionComponent params.region
        , createSizeComponent params.size
        , createRotationComponent params.rotation
        , createQualityComponent params.quality ++ "." ++ createFormatComponent params.format
        ]


{-| Render the region component for an image request.
-}
createRegionComponent : ImageRegion -> String
createRegionComponent region =
    case region of
        FullRegion ->
            "full"

        SquareRegion ->
            "square"

        SizeRegion { x, y, w, h } ->
            List.map String.fromInt [ x, y, w, h ]
                |> String.join ","

        PctSizeRegion { x, y, w, h } ->
            List.map String.fromFloat [ x, y, w, h ]
                |> String.join ","


{-| Render the size component for an image request.
-}
createSizeComponent : ImageSize -> String
createSizeComponent size =
    case size of
        MaxSize ->
            "max"

        ExactMaxSize ->
            "^max"

        WidthOnlySize w ->
            String.fromInt w ++ ","

        ExactWidthOnlySize w ->
            "^" ++ String.fromInt w ++ ","

        HeightOnlySize h ->
            "," ++ String.fromInt h

        ExactHeightOnlySize h ->
            "^," ++ String.fromInt h

        PercentSize p ->
            "pct:" ++ String.fromFloat p

        ExactPercentSize p ->
            "^pct:" ++ String.fromFloat p

        WidthAndHeightSize ( w, h ) ->
            String.fromInt w ++ "," ++ String.fromInt h

        ExactWidthAndHeightSize ( w, h ) ->
            "^" ++ String.fromInt w ++ "," ++ String.fromInt h

        ScaledWidthAndHeightSize ( w, h ) ->
            "!" ++ String.fromInt w ++ "," ++ String.fromInt h

        ExactScaledWidthAndHeightSize ( w, h ) ->
            "^!" ++ String.fromInt w ++ "," ++ String.fromInt h


{-| Render the rotation component for an image request.
-}
createRotationComponent : ImageRotation -> String
createRotationComponent rot =
    case rot of
        NormalRotation f ->
            String.fromFloat f

        MirroredRotation f ->
            "!" ++ String.fromFloat f


{-| Render the quality component for an image request.
-}
createQualityComponent : ImageQuality -> String
createQualityComponent qual =
    case qual of
        ColorQuality ->
            "color"

        GrayQuality ->
            "gray"

        BiTonalQuality ->
            "bitonal"

        DefaultQuality ->
            "default"

        NativeQuality ->
            "native"


{-| Render the format component for an image request.
-}
createFormatComponent : ImageFormat -> String
createFormatComponent fmt =
    case fmt of
        JpegFormat ->
            "jpg"

        TiffFormat ->
            "tif"

        PngFormat ->
            "png"

        Jp2Format ->
            "jp2"

        GifFormat ->
            "gif"

        PdfFormat ->
            "pdf"

        WebpFormat ->
            "webp"


{-| Quality/format suffixes used to detect Image API URLs.
-}
possibleSuffixes : List String
possibleSuffixes =
    let
        qualitySuffixes =
            [ "color", "gray", "bitonal", "default", "native" ]

        formatSuffixes =
            [ "jpg", "tif", "png", "gif", "jp2", "pdf", "webp" ]
    in
    cartesianProduct [ qualitySuffixes, formatSuffixes ]
        |> List.map (String.join ".")


{-| Parse an `Url` into a IIIF image or info.json URI.
-}
parseImageUrl : Url -> Maybe ImageUri
parseImageUrl { protocol, host, port_, path, query, fragment } =
    let
        protocolStr =
            case protocol of
                Http ->
                    "http://"

                Https ->
                    "https://"

        addr =
            Maybe.map (\p -> protocolStr ++ host ++ ":" ++ String.fromInt p) port_
                |> Maybe.withDefault (protocolStr ++ host)

        splitPath =
            String.split "/" path

        isImageApiUri =
            List.any (\s -> String.endsWith s path) possibleSuffixes
    in
    if isImageApiUri then
        imageUriFromComponents addr splitPath

    else
        infoUriFromComponents addr splitPath


{-| Construct a split path into an info.json URI. Useful if you have
a server and a list of prefix path components and you want to generate
an InfoUri record. Will remove any 'info.json' components that already exist.


    info =
        infoUriParser "https://example.org" [ "some", "prefix", "info.json" ]

    -- Just (InfoUri { host = "https://example.org", prefix = "some/prefix"]

-}
infoUriFromComponents : String -> List String -> Maybe ImageUri
infoUriFromComponents host pathComponents =
    let
        identifier =
            String.join "/" (remove "info.json" pathComponents)
    in
    Just (InfoUri { host = host, prefix = identifier })


{-| Parse a split path into an image request URI. Like infoUriParser, this
is useful if you have a server and a list of path components and want
an ImageUri record.


    img =
        imageUriParser "https://example.org"
            [ "some", "prefix", "full", "100,200", "20", "default.jpg" ]

    -- Just (ImageUri
    -- { host = "https://example.org"
    -- , prefix = "some/prefix"
    --, region = FullRegion
    --, size = WidthAndHeightSize ( 100, 200 )
    --, rotation = NormalRotation 20
    --, quality = DefaultQuality
    --, format = JpegFormat
    --- })

-}
imageUriFromComponents : String -> List String -> Maybe ImageUri
imageUriFromComponents host pathComponents =
    case List.reverse pathComponents of
        qualityFormat :: rotation :: size :: region :: ident ->
            let
                imageIdent =
                    List.reverse ident
                        |> String.join "/"
            in
            Just (ImageRequestParameters host imageIdent)
                |> applyMaybe (imageRegionParser region)
                |> applyMaybe (imageSizeParser size)
                |> applyMaybe (rotationParser rotation)
                |> applyMaybe (qualityParser qualityFormat)
                |> applyMaybe (formatParser qualityFormat)
                |> Maybe.map ImageUri

        _ ->
            Nothing


{-| Parse the quality from a `<quality>.<format>` string.
-}
qualityParser : String -> Maybe ImageQuality
qualityParser inp =
    case String.split "." inp of
        quality :: _ :: [] ->
            if quality == "default" then
                Just DefaultQuality

            else if quality == "native" then
                Just NativeQuality

            else if quality == "color" then
                Just ColorQuality

            else if quality == "gray" then
                Just GrayQuality

            else if quality == "bitonal" then
                Just BiTonalQuality

            else
                Nothing

        _ ->
            Nothing


{-| Parse the format from a `<quality>.<format>` string.
-}
formatParser : String -> Maybe ImageFormat
formatParser inp =
    case String.split "." inp of
        _ :: format :: [] ->
            if format == "jpg" then
                Just JpegFormat

            else if format == "tif" then
                Just TiffFormat

            else if format == "png" then
                Just PngFormat

            else if format == "gif" then
                Just GifFormat

            else if format == "jp2" then
                Just Jp2Format

            else if format == "pdf" then
                Just PdfFormat

            else if format == "webp" then
                Just WebpFormat

            else
                Nothing

        _ ->
            Nothing


{-| Parse a rotation component (normal or mirrored).
-}
rotationParser : String -> Maybe ImageRotation
rotationParser inp =
    oneOfMaybes
        [ normalRotationParser
        , mirroredRotationParser
        ]
        inp


{-| Parse a non-mirrored rotation value.
-}
normalRotationParser : String -> Maybe ImageRotation
normalRotationParser inp =
    String.toFloat inp
        |> Maybe.map NormalRotation


{-| Parse a mirrored rotation value (`!` prefix).
-}
mirroredRotationParser : String -> Maybe ImageRotation
mirroredRotationParser inp =
    if String.startsWith "!" inp then
        String.dropLeft 1 inp
            |> String.toFloat
            |> Maybe.map MirroredRotation

    else
        Nothing


{-| Parse the size component of an image request.
-}
imageSizeParser : String -> Maybe ImageSize
imageSizeParser inp =
    oneOfMaybes
        [ maxSizeParser
        , widthOnlySizeParser
        , exactWidthOnlySizeParser
        , heightOnlySizeParser
        , exactHeightOnlySizeParser
        , percentSizeParser
        , exactPercentSizeParser
        , widthAndHeightSizeParser
        , exactWidthAndHeightSizeParser
        , scaledWidthAndHeightSizeParser
        , exactScaledWidthAndHeightSizeParser
        ]
        inp


{-| Parse the `max`/`full` size variants.
-}
maxSizeParser : String -> Maybe ImageSize
maxSizeParser inp =
    if inp == "max" || inp == "full" then
        Just MaxSize

    else if inp == "^max" then
        Just ExactMaxSize

    else
        Nothing


{-| Parse a width-only size (`w,`).
-}
widthOnlySizeParser : String -> Maybe ImageSize
widthOnlySizeParser inp =
    if String.endsWith "," inp then
        String.dropRight 1 inp
            |> String.toInt
            |> Maybe.map WidthOnlySize

    else
        Nothing


{-| Parse an exact width-only size (`^w,`).
-}
exactWidthOnlySizeParser : String -> Maybe ImageSize
exactWidthOnlySizeParser inp =
    if String.endsWith "," inp && String.startsWith "^" inp then
        String.dropRight 1 inp
            |> String.dropLeft 1
            |> String.toInt
            |> Maybe.map ExactWidthOnlySize

    else
        Nothing


{-| Parse a height-only size (`,h`).
-}
heightOnlySizeParser : String -> Maybe ImageSize
heightOnlySizeParser inp =
    if String.startsWith "," inp then
        String.dropLeft 1 inp
            |> String.toInt
            |> Maybe.map HeightOnlySize

    else
        Nothing


{-| Parse an exact height-only size (`^,h`).
-}
exactHeightOnlySizeParser : String -> Maybe ImageSize
exactHeightOnlySizeParser inp =
    if String.startsWith "^," inp then
        String.dropLeft 2 inp
            |> String.toInt
            |> Maybe.map ExactHeightOnlySize

    else
        Nothing


{-| Parse a percent size (`pct:n`).
-}
percentSizeParser : String -> Maybe ImageSize
percentSizeParser inp =
    if String.startsWith "pct:" inp then
        String.dropLeft 4 inp
            |> String.toFloat
            |> Maybe.map PercentSize

    else
        Nothing


{-| Parse an exact percent size (`^pct:n`).
-}
exactPercentSizeParser : String -> Maybe ImageSize
exactPercentSizeParser inp =
    if String.startsWith "^pct:" inp then
        String.dropLeft 5 inp
            |> String.toFloat
            |> Maybe.map ExactPercentSize

    else
        Nothing


{-| Parse a `w,h` pair into a tuple.
-}
whSizeParser : String -> Maybe ( Int, Int )
whSizeParser inp =
    case String.split "," inp of
        width :: height :: [] ->
            Maybe.map2 Tuple.pair (String.toInt width) (String.toInt height)

        _ ->
            Nothing


{-| Parse a width and height size (`w,h`).
-}
widthAndHeightSizeParser : String -> Maybe ImageSize
widthAndHeightSizeParser inp =
    case String.split "," inp of
        _ :: _ :: [] ->
            whSizeParser inp
                |> Maybe.map WidthAndHeightSize

        _ ->
            Nothing


{-| Parse an exact width and height size (`^w,h`).
-}
exactWidthAndHeightSizeParser : String -> Maybe ImageSize
exactWidthAndHeightSizeParser inp =
    if String.startsWith "^" inp then
        String.dropLeft 1 inp
            |> whSizeParser
            |> Maybe.map ExactWidthAndHeightSize

    else
        Nothing


{-| Parse a scaled width and height size (`!w,h`).
-}
scaledWidthAndHeightSizeParser : String -> Maybe ImageSize
scaledWidthAndHeightSizeParser inp =
    if String.startsWith "!" inp then
        String.dropLeft 1 inp
            |> whSizeParser
            |> Maybe.map ScaledWidthAndHeightSize

    else
        Nothing


{-| Parse an exact scaled width and height size (`^!w,h`).
-}
exactScaledWidthAndHeightSizeParser : String -> Maybe ImageSize
exactScaledWidthAndHeightSizeParser inp =
    if String.startsWith "^!" inp then
        String.dropLeft 2 inp
            |> whSizeParser
            |> Maybe.map ExactScaledWidthAndHeightSize

    else
        Nothing


{-| Parse the region component of an image request.
-}
imageRegionParser : String -> Maybe ImageRegion
imageRegionParser inp =
    oneOfMaybes
        [ fullSquareRegionParser
        , sizeRegionParser
        , pctSizeRegionParser
        ]
        inp


{-| Parse the `full` or `square` region variants.
-}
fullSquareRegionParser : String -> Maybe ImageRegion
fullSquareRegionParser inp =
    if inp == "full" then
        Just FullRegion

    else if inp == "square" then
        Just SquareRegion

    else
        Nothing


{-| Parse a region expressed as `x,y,w,h`.
-}
sizeRegionParser : String -> Maybe ImageRegion
sizeRegionParser inp =
    let
        transformedInput =
            String.split "," inp
                |> List.filterMap String.toInt
    in
    case transformedInput of
        x :: y :: w :: h :: [] ->
            Just <| SizeRegion { x = x, y = y, w = w, h = h }

        _ ->
            Nothing


{-| Parse a region expressed as `pct:x,y,w,h`.
-}
pctSizeRegionParser : String -> Maybe ImageRegion
pctSizeRegionParser inp =
    let
        -- drops the `pct:` from the prefix. This should go last in the matching
        -- pipeline
        transformedInput =
            String.dropLeft 4 inp
                |> String.split ","
                |> List.filterMap String.toFloat
    in
    case transformedInput of
        x :: y :: w :: h :: [] ->
            Just <| PctSizeRegion { x = x, y = y, w = w, h = h }

        _ ->
            Nothing


{-| Parses an info.json URL and constructs an Image API Url that
returns an image that is 180px wide, suitable for showing a thumbnail image.

That is, it will take a URL like: `https://example.org/some/prefix/info.json` and
will return a URL like: `https://example.org/some/prefix/full/180,/0/default.jpg`

-}
thumbnailUrlFromInfo : String -> String
thumbnailUrlFromInfo infoUrl =
    case parseImageAddress infoUrl of
        Just iiifUrl ->
            setImageUriSize (WidthOnlySize 180) iiifUrl
                |> createImageAddress

        Nothing ->
            infoUrl
