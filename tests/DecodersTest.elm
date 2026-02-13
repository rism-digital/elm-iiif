module DecodersTest exposing (tests)

import Expect
import IIIF.Decoders exposing (infoJsonDecoder, manifestDecoder, resourceDecoder)
import IIIF.Image exposing (createImageAddress)
import IIIF.ImageInfo exposing (IIIFInfo(..))
import IIIF.Language exposing (Language(..), extractLabelFromLanguageMap)
import IIIF.Presentation exposing (IIIFManifest(..), IIIFResource(..))
import IIIF.Version exposing (IIIFVersion(..))
import Json.Decode as Decode
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Decoders"
        [ test "manifestDecoder parses minimal v3 manifest" <|
            \_ ->
                case Decode.decodeString manifestDecoder v3ManifestJson of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True (version == IIIFV3 && manifest.id == "https://example.org/manifest")

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "manifestDecoder parses minimal v2 manifest" <|
            \_ ->
                case Decode.decodeString manifestDecoder v2ManifestJson of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True (version == IIIFV2 && manifest.id == "https://example.org/manifest")

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "manifestDecoder parses v2 metadata with multilingual value object list" <|
            \_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonWithMultilingualMetadata of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version == IIIFV2
                                && manifest.id == "https://example.org/manifest"
                                && List.length manifest.metadata == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "manifestDecoder parses v2 metadata multilingual list for both label and value" <|
            \_ ->
                case Decode.decodeString manifestDecoder v2ManifestJsonWithMultilingualLabelAndValueMetadata of
                    Ok (IIIFManifest version manifest) ->
                        case List.head manifest.metadata of
                            Just metadata ->
                                Expect.equal True
                                    (version == IIIFV2
                                        && extractLabelFromLanguageMap (LanguageCode "de") metadata.label == "Titel"
                                        && extractLabelFromLanguageMap (LanguageCode "zh") metadata.value == "福音书"
                                    )

                            Nothing ->
                                Expect.fail "Expected one metadata entry"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "resourceDecoder parses v3 manifest resource" <|
            \_ ->
                case Decode.decodeString resourceDecoder v3ResourceJson of
                    Ok (ResourceManifest (IIIFManifest version manifest)) ->
                        Expect.equal True (version == IIIFV3 && manifest.id == "https://example.org/manifest")

                    Ok _ ->
                        Expect.fail "Expected ResourceManifest"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "resourceDecoder parses v2 manifest resource" <|
            \_ ->
                case Decode.decodeString resourceDecoder v2ResourceJson of
                    Ok (ResourceManifest (IIIFManifest version manifest)) ->
                        Expect.equal True (version == IIIFV2 && manifest.id == "https://example.org/manifest")

                    Ok _ ->
                        Expect.fail "Expected ResourceManifest"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "infoJsonDecoder parses v3 image info" <|
            \_ ->
                case Decode.decodeString infoJsonDecoder v3InfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True (version == IIIFV3 && info.width == 640 && info.height == 480)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "infoJsonDecoder parses v2 image info" <|
            \_ ->
                case Decode.decodeString infoJsonDecoder v2InfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True (version == IIIFV2 && info.width == 300 && info.height == 200)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "infoJsonDecoder parses real-world v2 info.json" <|
            \_ ->
                case Decode.decodeString infoJsonDecoder v2RealWorldInfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True
                            (version == IIIFV2
                                && info.width == 6676
                                && info.height == 8560
                                && createImageAddress info.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/image/36ebabd9-4d62-4d8e-8e7b-1afd048e872e/info.json"
                                && (Maybe.withDefault [] info.sizes |> List.length) == 6
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "infoJsonDecoder parses real-world v3 info.json" <|
            \_ ->
                case Decode.decodeString infoJsonDecoder v3RealWorldInfoJson of
                    Ok (IIIFInfo version info) ->
                        Expect.equal True
                            (version == IIIFV3
                                && info.width == 2363
                                && info.height == 2363
                                && createImageAddress info.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/image/f27e28db-0b08-4f16-9bdf-3565f591fb71/info.json"
                                && (Maybe.withDefault [] info.sizes |> List.length) == 4
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "manifestDecoder parses real-world v2 manifest" <|
            \_ ->
                case Decode.decodeString manifestDecoder v2RealWorldManifest of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version == IIIFV2
                                && manifest.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json"
                                && List.length manifest.canvases
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        , test "manifestDecoder parses real-world v3 manifest" <|
            \_ ->
                case Decode.decodeString manifestDecoder v3RealWorldManifest of
                    Ok (IIIFManifest version manifest) ->
                        Expect.equal True
                            (version == IIIFV3
                                && manifest.id
                                == "https://iiif.bodleian.ox.ac.uk/iiif/manifest/40824c0f-e1d5-4bc6-b051-aa66b0b7e1cc.json"
                                && List.length manifest.canvases
                                == 1
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]


v3ManifestJson : String
v3ManifestJson =
    "{\"@context\":\"http://iiif.io/api/presentation/3/context.json\",\"id\":\"https://example.org/manifest\",\"label\":{\"en\":[\"V3 Manifest\"]},\"items\":[{\"id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"items\":[{\"items\":[{\"body\":{\"id\":\"https://example.org/iiif/2/abc/info.json\",\"type\":\"Image\",\"service\":{\"id\":\"https://example.org/iiif/2/abc\",\"type\":\"ImageService3\"}}}]}]}]}"


v2ManifestJson : String
v2ManifestJson =
    "{\"@context\":\"http://iiif.io/api/presentation/2/context.json\",\"@id\":\"https://example.org/manifest\",\"label\":\"V2 Manifest\",\"sequences\":[{\"canvases\":[{\"@id\":\"https://example.org/canvas/1\",\"width\":100,\"height\":200,\"images\":[{\"resource\":{\"service\":{\"@id\":\"https://example.org/iiif/2/abc\",\"@context\":\"http://iiif.io/api/image/2/context.json\"}}}]}]}]}"


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
