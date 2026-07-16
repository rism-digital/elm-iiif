module DecodersTest exposing (tests)

import Expect
import IIIF.Decoders exposing (infoJsonDecoder, manifestDecoder, resourceDecoder)
import IIIF.Image exposing (ImageUri(..), createImageAddress, parseImageAddress)
import IIIF.ImageInfo exposing (ComplianceLevel(..), IIIFInfo(..))
import IIIF.Language exposing (Language(..), extractLabelFromLanguageMap)
import IIIF.Presentation exposing (IIIFManifest(..), IIIFResource(..))
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Decoders"
        [ test "manifestDecoder parses minimal v3 manifest"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJson of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True (version == IIIFV3 && manifest.id == "https://example.org/manifest")

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder accepts HTTPS v3 presentation contexts"
            (\_ ->
                case Decode.decodeString manifestDecoder (httpsIiifContexts v3ManifestJson) of
                    Ok (IIIFManifest version _) ->
                        Expect.equal IIIFV3 version

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v3 image body without an Image API service"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJsonPlainImage of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal True
                                    (version
                                        == IIIFV3
                                        && createImageAddress image.id
                                        == "https://example.org/image/plain.jpg"
                                        && List.isEmpty image.service
                                    )

                            Nothing ->
                                Expect.fail "Expected one image on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v3 canvas thumbnail without an Image API service"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJsonCanvasPlainThumbnail of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen .thumbnail of
                            Just image ->
                                Expect.equal True
                                    (version
                                        == IIIFV3
                                        && createImageAddress image.id
                                        == "https://example.org/thumb/plain.jpg"
                                        && List.isEmpty image.service
                                    )

                            Nothing ->
                                Expect.fail "Expected a thumbnail on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder treats v3 no-service IIIF-looking canvas thumbnail as static"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJsonCanvasIiifLookingThumbnail of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen .thumbnail of
                            Just image ->
                                case image.id of
                                    StaticImageUri _ ->
                                        Expect.equal True
                                            (version
                                                == IIIFV3
                                                && createImageAddress image.id
                                                == "https://example.org/iiif/2/abc/full/80,/0/default.jpg"
                                                && List.isEmpty image.service
                                            )

                                    _ ->
                                        Expect.fail "Expected StaticImageUri for no-service thumbnail"

                            Nothing ->
                                Expect.fail "Expected a thumbnail on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses minimal v2 manifest"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJson of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True (version == IIIFV2 && manifest.id == "https://example.org/manifest")

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder accepts HTTPS v2 presentation and embedded image contexts"
            (\_ ->
                case Decode.decodeString manifestDecoder (httpsIiifContexts v2ManifestJson) of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal ( IIIFV2, [ IIIF.Presentation.ImageService2 ] ) ( version, image.service )

                            Nothing ->
                                Expect.fail "Expected one image"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses Gallica Image API 1 services"
            (\_ ->
                case Decode.decodeString manifestDecoder gallicaV2ManifestJson of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal
                                    ( IIIFV2, [ IIIF.Presentation.ImageService1 ], "https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/f1/info.json" )
                                    ( version, image.service, createImageAddress image.id )

                            Nothing ->
                                Expect.fail "Expected one Gallica canvas image"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder accepts HTTPS Image API 1 service contexts"
            (\_ ->
                case Decode.decodeString manifestDecoder (httpsLegacyContexts gallicaV2ManifestJson) of
                    Ok (IIIFManifest _ manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal [ IIIF.Presentation.ImageService1 ] image.service

                            Nothing ->
                                Expect.fail "Expected one Gallica canvas image"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 image resource without an Image API service"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonPlainImage of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal True
                                    (version
                                        == IIIFV2
                                        && createImageAddress image.id
                                        == "https://example.org/image/plain.jpg"
                                        && List.isEmpty image.service
                                    )

                            Nothing ->
                                Expect.fail "Expected one image on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 canvas thumbnail without an Image API service"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonCanvasPlainThumbnail of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen .thumbnail of
                            Just image ->
                                Expect.equal True
                                    (version
                                        == IIIFV2
                                        && createImageAddress image.id
                                        == "https://example.org/thumb/plain.jpg"
                                        && List.isEmpty image.service
                                    )

                            Nothing ->
                                Expect.fail "Expected a thumbnail on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder treats v2 no-service IIIF-looking canvas thumbnail as static"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonCanvasIiifLookingThumbnail of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen .thumbnail of
                            Just image ->
                                case image.id of
                                    StaticImageUri _ ->
                                        Expect.equal True
                                            (version
                                                == IIIFV2
                                                && createImageAddress image.id
                                                == "https://example.org/iiif/2/abc/full/80,/0/default.jpg"
                                                && List.isEmpty image.service
                                            )

                                    _ ->
                                        Expect.fail "Expected StaticImageUri for no-service thumbnail"

                            Nothing ->
                                Expect.fail "Expected a thumbnail on the first canvas"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 canvas without images"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonCanvasWithoutImages of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version
                                == IIIFV2
                                && manifest.id
                                == "https://example.org/manifest"
                                && List.length manifest.canvases
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 canvas with only id type and label"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonCanvasLabelOnly of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version
                                == IIIFV2
                                && manifest.id
                                == "https://example.org/manifest"
                                && List.length manifest.canvases
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 metadata with multilingual value object list"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonWithMultilingualMetadata of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version
                                == IIIFV2
                                && manifest.id
                                == "https://example.org/manifest"
                                && List.length manifest.metadata
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses v2 metadata multilingual list for both label and value"
            (\_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonWithMultilingualLabelAndValueMetadata of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.metadata of
                            Just metadata ->
                                Expect.equal True
                                    (version
                                        == IIIFV2
                                        && extractLabelFromLanguageMap (LanguageCode "de") metadata.label
                                        == "Titel"
                                        && extractLabelFromLanguageMap (LanguageCode "zh") metadata.value
                                        == "福音书"
                                    )

                            Nothing ->
                                Expect.fail "Expected one metadata entry"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "resourceDecoder parses v3 manifest resource"
            (\_ ->
                case Decode.decodeString resourceDecoder v3ResourceJson of
                    Ok (ResourceManifest (IIIFManifest version manifest)) ->
                        Expect.equal True (version == IIIFV3 && manifest.id == "https://example.org/manifest")

                    Ok _ ->
                        Expect.fail "Expected ResourceManifest"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "resourceDecoder accepts HTTPS contexts"
            (\_ ->
                case Decode.decodeString resourceDecoder (httpsIiifContexts v3ResourceJson) of
                    Ok (ResourceManifest (IIIFManifest version _)) ->
                        Expect.equal IIIFV3 version

                    Ok _ ->
                        Expect.fail "Expected a v3 manifest resource"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "resourceDecoder parses v2 manifest resource"
            (\_ ->
                case Decode.decodeString resourceDecoder v2ResourceJson of
                    Ok (ResourceManifest (IIIFManifest version manifest)) ->
                        Expect.equal True (version == IIIFV2 && manifest.id == "https://example.org/manifest")

                    Ok _ ->
                        Expect.fail "Expected ResourceManifest"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder parses v3 image info"
            (\_ ->
                case Decode.decodeString infoJsonDecoder v3InfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True (version == IIIFV3 && info.width == 640 && info.height == 480 && info.profile == Nothing)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder accepts HTTPS v3 image contexts"
            (\_ ->
                case Decode.decodeString infoJsonDecoder (httpsIiifContexts v3InfoJson) of
                    Ok (IIIFInfo version _) ->
                        Expect.equal IIIFV3 version

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder parses v2 image info"
            (\_ ->
                case Decode.decodeString infoJsonDecoder v2InfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True (version == IIIFV2 && info.width == 300 && info.height == 200 && info.profile == Nothing)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder parses Gallica Image API 1.1 info"
            (\_ ->
                case Decode.decodeString infoJsonDecoder gallicaV1InfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal
                            { height = 4072
                            , id = "https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/f1/info.json"
                            , level = Just Level2
                            , version = IIIFV1
                            , width = 3055
                            }
                            { height = info.height
                            , id = createImageAddress info.id
                            , level = Maybe.map .complianceLevel info.profile
                            , version = version
                            , width = info.width
                            }

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder accepts HTTPS Image API 1.1 contexts"
            (\_ ->
                case Decode.decodeString infoJsonDecoder (httpsLegacyContexts gallicaV1InfoJson) of
                    Ok (IIIFInfo version _) ->
                        Expect.equal IIIFV1 version

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder normalizes Image API 1.1 tile and profile metadata"
            (\_ ->
                case Decode.decodeString infoJsonDecoder v1InfoJsonWithTiles of
                    Ok (IIIFInfo version info) ->
                        Expect.equal
                            { formats = Just [ "jpg", "png" ]
                            , qualities = Just [ "native", "grey" ]
                            , tiles = Just [ { width = 512, height = Just 256, scaleFactors = [ 1, 2, 4 ] } ]
                            , version = IIIFV1
                            }
                            { formats = info.profile |> Maybe.andThen .formats
                            , qualities = info.profile |> Maybe.andThen .qualities
                            , tiles = info.tiles
                            , version = version
                            }

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder recognizes Image API 1.1 compliance levels"
            (\_ ->
                [ "level0", "level1", "level2" ]
                    |> List.map
                        (\level ->
                            gallicaV1InfoJson
                                |> String.replace "level2" level
                                |> decodedComplianceLevel
                        )
                    |> Expect.equal [ Just Level0, Just Level1, Just Level2 ]
            )
        , test "infoJsonDecoder recognizes v3 levels and official HTTP/HTTPS v2 profiles"
            (\_ ->
                case
                    ( Decode.decodeString infoJsonDecoder v3InfoJsonWithProfile
                    , Decode.decodeString infoJsonDecoder v2InfoJsonWithProfile
                    , Decode.decodeString infoJsonDecoder (httpsIiifContexts v2InfoJsonWithProfile)
                    )
                of
                    ( Ok (IIIFInfo _ v3Info), Ok (IIIFInfo _ v2HttpInfo), Ok (IIIFInfo _ v2HttpsInfo) ) ->
                        Expect.equal
                            ( Just Level1, Just Level2, Just Level2 )
                            ( Maybe.map .complianceLevel v3Info.profile
                            , Maybe.map .complianceLevel v2HttpInfo.profile
                            , Maybe.map .complianceLevel v2HttpsInfo.profile
                            )

                    ( Err err, _, _ ) ->
                        Expect.fail (Decode.errorToString err)

                    ( _, Err err, _ ) ->
                        Expect.fail (Decode.errorToString err)

                    ( _, _, Err err ) ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder preserves unknown v2 and v3 profiles"
            (\_ ->
                case
                    ( Decode.decodeString infoJsonDecoder v3InfoJsonWithUnknownProfile
                    , Decode.decodeString infoJsonDecoder v2InfoJsonWithUnknownProfile
                    )
                of
                    ( Ok (IIIFInfo _ v3Info), Ok (IIIFInfo _ v2Info) ) ->
                        Expect.equal
                            ( Just (UnknownLevel "future"), Just (UnknownLevel "https://example.org/level1.json") )
                            ( Maybe.map .complianceLevel v3Info.profile, Maybe.map .complianceLevel v2Info.profile )

                    ( Err err, _ ) ->
                        Expect.fail (Decode.errorToString err)

                    ( _, Err err ) ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder rejects malformed present profiles and v2 details"
            (\_ ->
                Expect.equal True
                    (isDecodeFailure infoJsonDecoder v3InfoJsonWithMalformedProfile
                        && isDecodeFailure infoJsonDecoder v2InfoJsonWithMalformedProfile
                        && isDecodeFailure infoJsonDecoder v2InfoJsonWithMalformedProfileDetails
                        && isDecodeFailure infoJsonDecoder v2InfoJsonWithNonObjectProfileDetails
                    )
            )
        , test "infoJsonDecoder accepts HTTPS v2 image contexts"
            (\_ ->
                case Decode.decodeString infoJsonDecoder (httpsIiifContexts v2InfoJson) of
                    Ok (IIIFInfo version _) ->
                        Expect.equal IIIFV2 version

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder parses real-world v2 info.json"
            (\_ ->
                case Decode.decodeString infoJsonDecoder v2RealWorldInfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True
                            (version
                                == IIIFV2
                                && info.width
                                == 6676
                                && info.height
                                == 8560
                                && createImageAddress info.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/image/36ebabd9-4d62-4d8e-8e7b-1afd048e872e/info.json"
                                && (Maybe.withDefault [] info.sizes |> List.length)
                                == 6
                                && Maybe.map .complianceLevel info.profile
                                == Just Level2
                                && (info.profile |> Maybe.andThen .formats)
                                == Just [ "jpg", "png", "webp" ]
                                && (info.profile |> Maybe.andThen .maxWidth)
                                == Just 4000
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "infoJsonDecoder parses real-world v3 info.json"
            (\_ ->
                case Decode.decodeString infoJsonDecoder v3RealWorldInfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True
                            (version
                                == IIIFV3
                                && info.width
                                == 2363
                                && info.height
                                == 2363
                                && createImageAddress info.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71/info.json"
                                && (Maybe.withDefault [] info.sizes |> List.length)
                                == 4
                                && Maybe.map .complianceLevel info.profile
                                == Just Level2
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses real-world v2 manifest"
            (\_ ->
                case Decode.decodeString manifestDecoder v2RealWorldManifest of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version
                                == IIIFV2
                                && manifest.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json"
                                && List.length manifest.canvases
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder parses real-world v3 manifest"
            (\_ ->
                case Decode.decodeString manifestDecoder v3RealWorldManifest of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version
                                == IIIFV3
                                && manifest.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json"
                                && List.length manifest.canvases
                                == 1
                                && Maybe.map (createImageAddress << .id) manifest.thumbnail
                                == Just "https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db/info.json"
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder treats v3 no-service IIIF-looking manifest thumbnail as static"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJsonIiifLookingThumbnail of
                    Ok (IIIFManifest _ manifest) ->
                        case manifest.thumbnail of
                            Just image ->
                                case image.id of
                                    StaticImageUri _ ->
                                        Expect.equal "https://example.org/iiif/2/abc/full/80,/0/default.jpg" (createImageAddress image.id)

                                    _ ->
                                        Expect.fail "Expected StaticImageUri for no-service manifest thumbnail"

                            Nothing ->
                                Expect.fail "Expected manifest thumbnail"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "parseImageAddress round-trips static image urls"
            (\_ ->
                case parseImageAddress "https://example.org/image/plain.jpg?download=1" of
                    Just (StaticImageUri params) ->
                        Expect.equal "https://example.org/image/plain.jpg?download=1" (createImageAddress (StaticImageUri params))

                    Just _ ->
                        Expect.fail "Expected StaticImageUri"

                    Nothing ->
                        Expect.fail "Expected static image url to parse"
            )
        , test "manifestDecoder preserves malformed services without losing a valid static image"
            (\_ ->
                case Decode.decodeString manifestDecoder v3ManifestJsonMalformedService of
                    Ok (IIIFManifest _ manifest) ->
                        case List.head manifest.canvases |> Maybe.andThen (\canvas -> List.head canvas.images) of
                            Just image ->
                                Expect.equal ( 1, 1 ) ( List.length image.service, List.length image.serviceObjects )

                            Nothing ->
                                Expect.fail "Expected one image"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "manifestDecoder still rejects unknown context hosts"
            (\_ ->
                case Decode.decodeString manifestDecoder (String.replace "iiif.io" "example.com" v3ManifestJson) of
                    Ok _ ->
                        Expect.fail "Expected an unknown context host to fail"

                    Err _ ->
                        Expect.pass
            )
        ]


httpsIiifContexts : String -> String
httpsIiifContexts =
    String.replace "http://iiif.io/api/" "https://iiif.io/api/"


httpsLegacyContexts : String -> String
httpsLegacyContexts json =
    json
        |> String.replace "http://iiif.io/api/image/1/" "https://iiif.io/api/image/1/"
        |> String.replace "http://library.stanford.edu/iiif/image-api/1.1/" "https://library.stanford.edu/iiif/image-api/1.1/"


decodedComplianceLevel : String -> Maybe ComplianceLevel
decodedComplianceLevel json =
    case Decode.decodeString infoJsonDecoder json of
        Ok (IIIFInfo _ info) ->
            Maybe.map .complianceLevel info.profile

        Err _ ->
            Nothing


isDecodeFailure : Decode.Decoder a -> String -> Bool
isDecodeFailure decoder json =
    case Decode.decodeString decoder json of
        Ok _ ->
            False

        Err _ ->
            True


v3ManifestJson : String
v3ManifestJson =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/iiif/2/abc/info.json\",\"type\":\"Image\",\"service\":{\"id\":\"https://example.org/iiif/2/abc\",\"type\":\"ImageService3\"}}}]}]}]}"


v3ManifestJsonPlainImage : String
v3ManifestJsonPlainImage =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/image/plain.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v3ManifestJsonCanvasPlainThumbnail : String
v3ManifestJsonCanvasPlainThumbnail =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"thumbnail\":[{\"id\":\"https://example.org/thumb/plain.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}],\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/image/plain.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v3ManifestJsonCanvasIiifLookingThumbnail : String
v3ManifestJsonCanvasIiifLookingThumbnail =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"thumbnail\":[{\"id\":\"https://example.org/iiif/2/abc/full/80,/0/default.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}],\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/image/plain.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v3ManifestJsonIiifLookingThumbnail : String
v3ManifestJsonIiifLookingThumbnail =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"thumbnail\":[{\"id\":\"https://example.org/iiif/2/abc/full/80,/0/default.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}],\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/image/plain.jpg\",\"type\":\"Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v3ManifestJsonMalformedService : String
v3ManifestJsonMalformedService =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/image/plain.jpg\",\"type\":\"Image\",\"service\":{\"type\":\"ImageService3\"}}}]}]}]}"


v2ManifestJson : String
v2ManifestJson =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"service\":{\"@id\":\"https://example.org/iiif/2/abc\",\"@context\":\"http://iiif.io/api/image/2/context.json\"}}}]}]}]}"


gallicaV2ManifestJson : String
gallicaV2ManifestJson =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/manifest.json\",\"@type\":\"sc:Manifest\",\"label\":\"BnF, département Musique, RES VM7-676\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/canvas/f1\",\"label\":\"Plat supérieur\",\"height\":4072,\"width\":3055,\"images\":[{\"motivation\":\"sc:painting\",\"resource\":{\"format\":\"image/jpeg\",\"service\":{\"profile\":\"http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level2\",\"@context\":\"http://iiif.io/api/image/1/context.json\",\"@id\":\"https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/f1\"},\"height\":4072,\"width\":3055,\"@id\":\"https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/f1/full/full/0/native.jpg\",\"@type\":\"dctypes:Image\"}}]}]}]}"


v2ManifestJsonPlainImage : String
v2ManifestJsonPlainImage =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"@id\":\"https://example.org/image/plain.jpg\",\"@type\":\"dctypes:Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v2ManifestJsonCanvasPlainThumbnail : String
v2ManifestJsonCanvasPlainThumbnail =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"thumbnail\":{\"@id\":\"https://example.org/thumb/plain.jpg\",\"@type\":\"dctypes:Image\",\"format\":\"image/jpeg\"},\"images\":[{\"resource\":{\"@id\":\"https://example.org/image/plain.jpg\",\"@type\":\"dctypes:Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v2ManifestJsonCanvasIiifLookingThumbnail : String
v2ManifestJsonCanvasIiifLookingThumbnail =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"thumbnail\":{\"@id\":\"https://example.org/iiif/2/abc/full/80,/0/default.jpg\",\"@type\":\"dctypes:Image\",\"format\":\"image/jpeg\"},\"images\":[{\"resource\":{\"@id\":\"https://example.org/image/plain.jpg\",\"@type\":\"dctypes:Image\",\"format\":\"image/jpeg\"}}]}]}]}"


v2ManifestJsonCanvasWithoutImages : String
v2ManifestJsonCanvasWithoutImages =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"label\":\"No images canvas\",\"width\":100,\"height\":200}]}]}"


v2ManifestJsonCanvasLabelOnly : String
v2ManifestJsonCanvasLabelOnly =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"@type\":\"sc:Canvas\",\"label\":\"Label only canvas\"}]}]}"


v2ManifestJsonWithMultilingualMetadata : String
v2ManifestJsonWithMultilingualMetadata =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"metadata\":[{\"label\":[{\"@language\":\"en\",\"@value\":\"Title\"},{\"@language\":\"de\",\"@value\":\"Titel\"},{\"@language\":\"zh\",\"@value\":\"書名\"}],\"value\":\"Evangelistar: Perikopenbuch Heinrichs II. - BSB Clm 4452\"}],\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"service\":{\"@id\":\"https://example.org/iiif/2/abc\",\"@context\":\"http://iiif.io/api/image/2/context.json\"}}}]}]}]}"


v2ManifestJsonWithMultilingualLabelAndValueMetadata : String
v2ManifestJsonWithMultilingualLabelAndValueMetadata =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"metadata\":[{\"label\":[{\"@language\":\"en\",\"@value\":\"Title\"},{\"@language\":\"de\",\"@value\":\"Titel\"},{\"@language\":\"zh\",\"@value\":\"書名\"}],\"value\":[{\"@language\":\"en\",\"@value\":\"Book of Gospels\"},{\"@language\":\"de\",\"@value\":\"Evangeliar\"},{\"@language\":\"zh\",\"@value\":\"福音书\"}]}],\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"service\":{\"@id\":\"https://example.org/iiif/2/abc\",\"@context\":\"http://iiif.io/api/image/2/context.json\"}}}]}]}]}"


v3ResourceJson : String
v3ResourceJson =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"type\":\"Manifest\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/iiif/2/abc/info.json\",\"type\":\"Image\",\"service\":{\"id\":\"https://example.org/iiif/2/abc\",\"type\":\"ImageService3\"}}}]}]}]}"


v2ResourceJson : String
v2ResourceJson =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@type\":\"sc:Manifest\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"service\":{\"@id\":\"https://example.org/iiif/2/abc\",\"@context\":\"http://iiif.io/api/image/2/context.json\"}}}]}]}]}"


v3InfoJson : String
v3InfoJson =
    "{\"@context\":\"http://iiif.io/api/image/3/context.json\",\"id\":\"https://example.org/iiif/2/abc/info.json\",\"width\":640,\"height\":480}"


v2InfoJson : String
v2InfoJson =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc/info.json\",\"width\":300,\"height\":200}"


gallicaV1InfoJson : String
gallicaV1InfoJson =
    "{\"@context\":\"http://library.stanford.edu/iiif/image-api/1.1/context.json\",\"@id\":\"https://gallica.bnf.fr/iiif/ark:/12148/btv1b550082258/f1\",\"width\":3055,\"height\":4072,\"profile\":\"http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level2\"}"


v1InfoJsonWithTiles : String
v1InfoJsonWithTiles =
    "{\"@context\":\"http://library.stanford.edu/iiif/image-api/1.1/context.json\",\"@id\":\"https://example.org/iiif/1/abc\",\"width\":3000,\"height\":2000,\"scale_factors\":[1,2,4],\"tile_width\":512,\"tile_height\":256,\"formats\":[\"jpg\",\"png\"],\"qualities\":[\"native\",\"grey\"],\"profile\":\"http://library.stanford.edu/iiif/image-api/1.1/compliance.html#level1\"}"


v3InfoJsonWithProfile : String
v3InfoJsonWithProfile =
    "{\"@context\":\"http://iiif.io/api/image/3/context.json\",\"id\":\"https://example.org/iiif/3/abc\",\"width\":640,\"height\":480,\"profile\":\"level1\"}"


v2InfoJsonWithProfile : String
v2InfoJsonWithProfile =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc\",\"width\":300,\"height\":200,\"profile\":\"http://iiif.io/api/image/2/level2.json\"}"


v3InfoJsonWithUnknownProfile : String
v3InfoJsonWithUnknownProfile =
    "{\"@context\":\"http://iiif.io/api/image/3/context.json\",\"id\":\"https://example.org/iiif/3/abc\",\"width\":640,\"height\":480,\"profile\":\"future\"}"


v2InfoJsonWithUnknownProfile : String
v2InfoJsonWithUnknownProfile =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc\",\"width\":300,\"height\":200,\"profile\":\"https://example.org/level1.json\"}"


v3InfoJsonWithMalformedProfile : String
v3InfoJsonWithMalformedProfile =
    "{\"@context\":\"http://iiif.io/api/image/3/context.json\",\"id\":\"https://example.org/iiif/3/abc\",\"width\":640,\"height\":480,\"profile\":1}"


v2InfoJsonWithMalformedProfile : String
v2InfoJsonWithMalformedProfile =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc\",\"width\":300,\"height\":200,\"profile\":{}}"


v2InfoJsonWithMalformedProfileDetails : String
v2InfoJsonWithMalformedProfileDetails =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc\",\"width\":300,\"height\":200,\"profile\":[\"http://iiif.io/api/image/2/level2.json\",{\"formats\":\"jpg\"}]}"


v2InfoJsonWithNonObjectProfileDetails : String
v2InfoJsonWithNonObjectProfileDetails =
    "{\"@context\":\"http://iiif.io/api/image/2/context.json\",\"@id\":\"https://example.org/iiif/2/abc\",\"width\":300,\"height\":200,\"profile\":[\"http://iiif.io/api/image/2/level2.json\",\"invalid\"]}"


v2RealWorldManifest : String
v2RealWorldManifest =
    """{"@context":"http://iiif.io/api/presentation/2/context.json","@id":"https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json","@type":"sc:Manifest","label":"Bodleian Library LP 156","description":"Portrait of Elizabeth, Princess Palatine (1618–1680)","metadata":[{"label":"Homepage","value":"<span><a href=\\"https://digital.bodleian.ox.ac.uk/objects/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc/\\">View on Digital Bodleian</a></span>"},{"label":"Title","value":"Portrait of Elizabeth, Princess Palatine (1618–1680)"},{"label":"Shelfmark","value":"Bodleian Library LP 156"},{"label":"Artist","value":"Artist unknown"},{"label":"Artist (Follower of)","value":"Gerrit van Honthorst (1590-1656)"},{"label":"Sitter","value":"Elizabeth, Princess Palatine (1618-1680)"},{"label":"Language","value":"No linguistic content"},{"label":"Date Statement","value":"17th century"},{"label":"Materials","value":"oil on canvas"},{"label":"Dimensions","value":"737 × 602 mm."},{"label":"Provenance","value":"Given by Dr Richard Rawlinson, 1748/9."},{"label":"Accession Date","value":"1748"},{"label":"Accession Source","value":"Richard Rawlinson (1690-1755)"},{"label":"Accession Type","value":"gift"},{"label":"Record Origin","value":"Description by Dana Josephson (2019)."},{"label":"Collection","value":"Portraits"},{"label":"Additional Information Sources","value":"Poole, Rachael. Catalogue of portraits in the possession of the University, colleges, city, and county of Oxford (Oxford, 1912). Garlick, Kenneth, and Rachael Poole. Catalogue of portraits in the Bodleian Library, Oxford (Oxford, 2004)."},{"label":"Digitization Project","value":"The Bodleian Libraries’ Portrait Collection: A Samuel H. Kress Foundation Digitization Project"},{"label":"Record Created","value":"2019-06-17T15:42:36Z"},{"label":"Holding Institution","value":"Bodleian Libraries, University of Oxford"},{"label":"Digitization Sponsor","value":"<span>The Samuel H. Kress Foundation</span>"}],"navDate":"1600-01-01T00:00:00Z","rendering":{"@id":"https://digital.bodleian.ox.ac.uk/objects/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc/","label":"View on Digital Bodleian","format":"text/html"},"attribution":"<span>Photo: © Bodleian Libraries, University of Oxford. Terms of use: <a href=\\"https://creativecommons.org/licenses/by-nc/4.0/\\">CC BY-NC 4.0</a>. For more information, please see <a href=\\"https://digital.bodleian.ox.ac.uk/terms/\\">https://digital.bodleian.ox.ac.uk/terms/</a></span>","logo":{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71/full/256,/0/default.jpg","@type":"dctypes:Image","format":"image/jpeg","service":{"@context":"http://iiif.io/api/image/2/context.json","profile":"http://iiif.io/api/image/2/level1.json","@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71"}},"thumbnail":{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db/full/256,/0/default.jpg","@type":"dctypes:Image","format":"image/jpeg","service":{"@context":"http://iiif.io/api/image/2/context.json","profile":"http://iiif.io/api/image/2/level1.json","@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db"}},"viewingHint":"paged","viewingDirection":"left-to-right","sequences":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/sequence/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc_default.json","@type":"sc:Sequence","label":"Default","canvases":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","@type":"sc:Canvas","label":"front","width":2195,"height":2707,"images":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/annotation/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","@type":"oa:Annotation","motivation":"sc:painting","on":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","resource":{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","@type":"dctypes:Image","format":"image/jpeg","width":2195,"height":2707,"service":{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","@context":"http://iiif.io/api/image/2/context.json","profile":"http://iiif.io/api/image/2/level1.json"}}}],"otherContent":[]}]}],"structures":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/range/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc/LOG_0000","@type":"sc:Range","label":"LP 156","viewingHint":"top","canvases":["https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json"],"metadata":[],"startCanvas":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json"}]}"""


v3RealWorldManifest : String
v3RealWorldManifest =
    """{"@context":"http://iiif.io/api/presentation/3/context.json","id":"https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json","type":"Manifest","label":{"en":["Bodleian Library LP 156"]},"summary":{"en":["Portrait of Elizabeth, Princess Palatine (1618–1680)"]},"metadata":[{"label":{"en":["Title"]},"value":{"en":["Portrait of Elizabeth, Princess Palatine (1618–1680)"]}},{"label":{"en":["Shelfmark"]},"value":{"en":["Bodleian Library LP 156"]}},{"label":{"en":["Artist"]},"value":{"en":["Artist unknown"]}},{"label":{"en":["Artist (Follower of)"]},"value":{"en":["Gerrit van Honthorst (1590-1656)"]}},{"label":{"en":["Sitter"]},"value":{"en":["Elizabeth, Princess Palatine (1618-1680)"]}},{"label":{"en":["Language"]},"value":{"en":["No linguistic content"]}},{"label":{"en":["Date Statement"]},"value":{"en":["17th century"]}},{"label":{"en":["Materials"]},"value":{"en":["oil on canvas"]}},{"label":{"en":["Dimensions"]},"value":{"en":["737 × 602 mm."]}},{"label":{"en":["Provenance"]},"value":{"en":["Given by Dr Richard Rawlinson, 1748/9."]}},{"label":{"en":["Accession Date"]},"value":{"en":["1748"]}},{"label":{"en":["Accession Source"]},"value":{"en":["Richard Rawlinson (1690-1755)"]}},{"label":{"en":["Accession Type"]},"value":{"en":["gift"]}},{"label":{"en":["Record Origin"]},"value":{"en":["Description by Dana Josephson (2019)."]}},{"label":{"en":["Collection"]},"value":{"en":["Portraits"]}},{"label":{"en":["Additional Information Sources"]},"value":{"en":["Poole, Rachael. Catalogue of portraits in the possession of the University, colleges, city, and county of Oxford (Oxford, 1912). Garlick, Kenneth, and Rachael Poole. Catalogue of portraits in the Bodleian Library, Oxford (Oxford, 2004)."]}},{"label":{"en":["Digitization Project"]},"value":{"en":["The Bodleian Libraries’ Portrait Collection: A Samuel H. Kress Foundation Digitization Project"]}},{"label":{"en":["Record Created"]},"value":{"en":["2019-06-17T15:42:36Z"]}},{"label":{"en":["Holding Institution"]},"value":{"en":["Bodleian Libraries, University of Oxford"]}},{"label":{"en":["Access Rights"]},"value":{"en":["Photo: © Bodleian Libraries, University of Oxford"]}},{"label":{"en":["Digitization Sponsor"]},"value":{"en":["<span>The Samuel H. Kress Foundation</span>"]}}],"homepage":[{"id":"https://digital.bodleian.ox.ac.uk/objects/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc/","type":"Text","label":{"en":["View on Digital Bodleian"]},"format":"text/html","language":["en"]}],"provider":[{"id":"https://viaf.org/viaf/173632201/","type":"Agent","label":{"en":["Bodleian Libraries, University of Oxford"]},"homepage":[{"id":"https://www.bodleian.ox.ac.uk/","type":"Text","label":{"en":["Bodleian Libraries, University of Oxford"]},"format":"text/html"}],"logo":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71/full/256,/0/default.jpg","type":"Image","service":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71","@type":"ImageService2","profile":"http://iiif.io/api/image/2/level1.json"},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71","type":"ImageService3","profile":"level1"}]}]}],"navDate":"1600-01-01T00:00:00Z","thumbnail":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db/full/256,/0/default.jpg","type":"Image","service":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","@type":"ImageService2","profile":"http://iiif.io/api/image/2/level1.json"},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","type":"ImageService3","profile":"level1"}]}],"requiredStatement":{"label":{"en":["Terms of Use"]},"value":{"en":["<span>Terms of use: <a href=\\"https://creativecommons.org/licenses/by-nc/4.0/\\">CC BY-NC 4.0</a>. For more information, please see <a href=\\"https://digital.bodleian.ox.ac.uk/terms/\\">https://digital.bodleian.ox.ac.uk/terms/</a></span>"]}},"partOf":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/collection/portraits","type":"Collection","label":{"en":["Portraits"]}},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/collection/bodleian","type":"Collection","label":{"en":["Bodleian Libraries"]}},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/collection/portraits-prints-drawings-objects","type":"Collection","label":{"en":["Portraits, Prints and Drawings"]}},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/collection/bodleian-portraits","type":"Collection","label":{"en":["The Bodleian Libraries’ Portrait Collection: A Samuel H. Kress Foundation Digitization Project"]}}],"behavior":["paged"],"items":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","type":"Canvas","label":{"en":["front"]},"width":2195,"height":2707,"items":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/annotationpage/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","type":"AnnotationPage","items":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/annotation/9cd10055-3c91-47f6-a3e9-04e5d8b199db_image.json","type":"Annotation","target":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","body":{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db/full/max/0/default.jpg","type":"Image","format":"image/jpeg","width":2195,"height":2707,"service":[{"@id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","@type":"ImageService2","profile":"http://iiif.io/api/image/2/level1.json"},{"id":"https://iiif.bodleian.ox.ac.uk/iiif/image/9cd10055-3c91-47f6-a3e9-04e5d8b199db","type":"ImageService3","profile":"level1"}]},"motivation":"painting"}]}]}],"structures":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/range/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc/LOG_0000","type":"Range","label":{"en":["LP 156"]},"metadata":[],"items":[{"id":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","type":"Canvas"}],"start":{"id":"https://iiif.bodleian.ox.ac.uk/iiif/canvas/9cd10055-3c91-47f6-a3e9-04e5d8b199db.json","type":"Canvas"}}],"viewingDirection":"left-to-right"}"""


v2RealWorldInfoJson : String
v2RealWorldInfoJson =
    """{
         "@context" : "http://iiif.io/api/image/2/context.json",
         "protocol" : "http://iiif.io/api/image",
         "width" : 6676,
         "height" : 8560,
         "sizes" : [
            { "width" : 104, "height" : 133 },
            { "width" : 208, "height" : 267 },
            { "width" : 417, "height" : 535 },
            { "width" : 834, "height" : 1070 },
            { "width" : 1669, "height" : 2140 },
            { "width" : 3338, "height" : 4280 }
         ],
         "tiles" : [
            { "width" : 256, "height" : 256, "scaleFactors" : [ 1, 2, 4, 8, 16, 32, 64 ] }
         ],
         "@id" : "https://iiif.bodleian.ox.ac.uk/iiif/image/36ebabd9-4d62-4d8e-8e7b-1afd048e872e",
         "profile" : [
            "http://iiif.io/api/image/2/level2.json",
            { "formats" : [ "jpg", "png", "webp" ],
              "qualities" : ["native","color","gray","bitonal"],
              "supports" : ["regionByPct","regionSquare","sizeByForcedWh","sizeByWh","sizeAboveFull","sizeUpscaling","rotationBy90s","mirroring"],
              "maxWidth" : 4000,
              "maxHeight" : 4000
            }
         ],
         "service": [
           {
             "@context": "http://iiif.io/api/annex/services/physdim/1/context.json",
             "profile": "http://iiif.io/api/annex/services/physdim",
             "physicalScale": 0.00423948,
             "physicalUnits": "cm"
           }
         ]
       }"""


v3RealWorldInfoJson : String
v3RealWorldInfoJson =
    """{
           "@context": "http://iiif.io/api/image/3/context.json",
           "protocol": "http://iiif.io/api/image",
           "width": 2363,
           "height": 2363,
           "sizes": [
               {
                   "width": 147,
                   "height": 147
               },
               {
                   "width": 295,
                   "height": 295
               },
               {
                   "width": 590,
                   "height": 590
               },
               {
                   "width": 1181,
                   "height": 1181
               }
           ],
           "tiles": [
               {
                   "width": 256,
                   "height": 256,
                   "scaleFactors": [
                       1,
                       2,
                       4,
                       8,
                       16
                   ]
               }
           ],
           "id": "https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71",
           "type": "ImageService3",
           "profile": "level2",
           "maxWidth": 4000,
           "maxHeight": 4000,
           "extraQualities": [
               "color",
               "gray",
               "bitonal"
           ],
           "extraFormats": [
               "webp"
           ],
           "extraFeatures": [
               "regionByPct",
               "sizeByForcedWh",
               "sizeByWh",
               "sizeAboveFull",
               "sizeUpscaling",
               "rotationBy90s",
               "mirroring"
           ],
           "service": [
               {
                   "@context": "http://iiif.io/api/annex/services/physdim/1/context.json",
                   "profile": "http://iiif.io/api/annex/services/physdim",
                   "physicalScale": 0.00846668,
                   "physicalUnits": "cm"
               }
           ],
           "preferredFormats": [
               "webp"
           ]
       }
"""
