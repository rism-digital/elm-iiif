module ImageTest exposing (tests)

import Expect
import IIIF.Image
    exposing
        ( ImageFormat(..)
        , ImageQuality(..)
        , ImageRegion(..)
        , ImageRotation(..)
        , ImageSize(..)
        , createImageUri
        , parseImageAddress
        , thumbnailUrlFromInfo
        )
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Image"
        [ test "createImageUri renders full image request" <|
            \_ ->
                let
                    params =
                        { host = "https://example.org"
                        , prefix = "/iiif/2/abc"
                        , region = FullRegion
                        , size = WidthAndHeightSize ( 100, 200 )
                        , rotation = NormalRotation 0
                        , quality = DefaultQuality
                        , format = JpegFormat
                        }
                in
                createImageUri params
                    |> Expect.equal "https://example.org/iiif/2/abc/full/100,200/0/default.jpg"
        , test "parseImageAddress parses info.json urls" <|
            \_ ->
                case parseImageAddress "https://example.org/iiif/2/abc/info.json" of
                    Just (IIIF.Image.InfoUri params) ->
                        Expect.equal
                            { host = "https://example.org", prefix = "/iiif/2/abc" }
                            params

                    _ ->
                        Expect.fail "Expected InfoUri"
        , test "parseImageAddress parses image urls" <|
            \_ ->
                case parseImageAddress "https://example.org/iiif/2/abc/full/100,200/0/default.jpg" of
                    Just (IIIF.Image.ImageUri params) ->
                        Expect.equal
                            { host = "https://example.org"
                            , prefix = "/iiif/2/abc"
                            , region = FullRegion
                            , size = WidthAndHeightSize ( 100, 200 )
                            , rotation = NormalRotation 0
                            , quality = DefaultQuality
                            , format = JpegFormat
                            }
                            params

                    _ ->
                        Expect.fail "Expected ImageUri"
        , test "thumbnailUrlFromInfo produces 180px-wide image url" <|
            \_ ->
                thumbnailUrlFromInfo "https://example.org/iiif/2/abc/info.json"
                    |> Expect.equal "https://example.org/iiif/2/abc/full/180,/0/default.jpg"
        ]
