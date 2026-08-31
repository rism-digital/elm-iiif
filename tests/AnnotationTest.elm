module AnnotationTest exposing (tests)

import Expect
import IIIF.Annotation exposing (AnnotationSelector(..), decodePage)
import Json.Decode as Decode
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Annotation"
        [ test "decodes a v3 AnnotationPage with a SpecificResource rectangle target"
            (\_ ->
                case Decode.decodeString decodePage v3RectanglePage of
                    Ok [ annotation ] ->
                        Expect.equal
                            { bodyValue = "A comment"
                            , format = Just "text/html"
                            , language = Just "en"
                            , selector = Rectangle 300 800 1200 1200
                            , source = Just "https://example.org/canvas/1"
                            }
                            { bodyValue = annotation.body.value
                            , format = annotation.body.format
                            , language = annotation.body.language
                            , selector = annotation.target.selector
                            , source = annotation.target.source
                            }

                    Ok _ ->
                        Expect.fail "Expected one annotation"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "decodes a v2 resource with a string target"
            (\_ ->
                case Decode.decodeString decodePage v2RectanglePage of
                    Ok [ annotation ] ->
                        Expect.equal
                            ( Just "https://example.org/canvas/2", Rectangle 10 20 30 40, "A legacy comment" )
                            ( annotation.target.source, annotation.target.selector, annotation.body.value )

                    Ok _ ->
                        Expect.fail "Expected one annotation"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "decodes an SVG selector nested in a Choice"
            (\_ ->
                case Decode.decodeString decodePage svgChoicePage of
                    Ok [ annotation ] ->
                        Expect.equal
                            (Svg "<svg><path d=\"M 0 0 L 5 5\"/></svg>")
                            annotation.target.selector

                    Ok _ ->
                        Expect.fail "Expected one annotation"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        ]


v3RectanglePage : String
v3RectanglePage =
    """{
      "type": "AnnotationPage",
      "items": [{
        "id": "https://example.org/annotation/1",
        "motivation": "commenting",
        "body": { "value": "A comment", "format": "text/html", "language": "en" },
        "target": {
          "type": "SpecificResource",
          "source": { "id": "https://example.org/canvas/1", "type": "Canvas" },
          "selector": { "type": "FragmentSelector", "value": "xywh=300,800,1200,1200" }
        }
      }]
    }"""


v2RectanglePage : String
v2RectanglePage =
    """{
      "resources": [{
        "@id": "https://example.org/annotation/2",
        "resource": { "chars": "A legacy comment" },
        "on": "https://example.org/canvas/2#xywh=10,20,30,40"
      }]
    }"""


svgChoicePage : String
svgChoicePage =
    """{
      "items": [{
        "id": "https://example.org/annotation/3",
        "body": "An SVG annotation",
        "target": {
          "source": "https://example.org/canvas/3",
          "selector": {
            "type": "Choice",
            "default": {
              "type": "SvgSelector",
              "value": "<svg><path d=\\"M 0 0 L 5 5\\"/></svg>"
            }
          }
        }
      }]
    }"""
