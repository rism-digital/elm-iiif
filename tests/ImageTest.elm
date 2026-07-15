module ImageTest exposing (tests)

import Expect
import IIIF.Image
    exposing
        ( ImageFormat(..)
        , ImageQuality(..)
        , ImageRegion(..)
        , ImageRotation(..)
        , ImageSize(..)
        , ImageUri(..)
        , createImageAddress
        , createImageUri
        , createInfoUri
        , parseImageAddress
        , thumbnailUrlFromInfo
        )
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Image"
        [ test "createImageUri renders full image request"
            (\_ ->
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
            )
        , test "parseImageAddress parses info.json urls"
            (\_ ->
                case parseImageAddress "https://example.org/iiif/2/abc/info.json" of
                    Just (IIIF.Image.InfoUri params) ->
                        Expect.equal { host = "https://example.org", prefix = "/iiif/2/abc" }
                            params

                    _ ->
                        Expect.fail "Expected InfoUri"
            )
        , test "parseImageAddress parses image urls"
            (\_ ->
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
            )
        , test "thumbnailUrlFromInfo produces 180px-wide image url" (\_ -> thumbnailUrlFromInfo "https://example.org/iiif/2/abc/info.json" |> Expect.equal "https://example.org/iiif/2/abc/full/180,/0/default.jpg")
        , test "createInfoUri removes duplicate host and prefix boundary slashes"
            (\_ ->
                createInfoUri
                    { host = "https://example.org///"
                    , prefix = "///iiif/2/abc///"
                    }
                    |> Expect.equal "https://example.org/iiif/2/abc/info.json"
            )
        , test "createInfoUri accepts a prefix without a leading slash"
            (\_ ->
                createInfoUri
                    { host = "https://example.org"
                    , prefix = "iiif/2/abc"
                    }
                    |> Expect.equal "https://example.org/iiif/2/abc/info.json"
            )
        , test "createInfoUri supports an empty prefix"
            (\_ ->
                createInfoUri
                    { host = "https://example.org/"
                    , prefix = ""
                    }
                    |> Expect.equal "https://example.org/info.json"
            )
        , test "createImageUri removes duplicate boundary slashes"
            (\_ ->
                createImageUri
                    { host = "https://example.org///"
                    , prefix = "///iiif/2/abc///"
                    , region = FullRegion
                    , size = WidthAndHeightSize ( 100, 200 )
                    , rotation = NormalRotation 0
                    , quality = DefaultQuality
                    , format = JpegFormat
                    }
                    |> Expect.equal "https://example.org/iiif/2/abc/full/100,200/0/default.jpg"
            )
        , test "createImageAddress normalizes InfoUri boundaries"
            (\_ ->
                InfoUri { host = "https://example.org/", prefix = "/iiif/2/abc/" }
                    |> createImageAddress
                    |> Expect.equal "https://example.org/iiif/2/abc/info.json"
            )
        , test "createImageAddress normalizes ImageUri boundaries"
            (\_ ->
                ImageUri
                    { host = "https://example.org/"
                    , prefix = "/iiif/2/abc/"
                    , region = FullRegion
                    , size = MaxSize
                    , rotation = NormalRotation 0
                    , quality = DefaultQuality
                    , format = JpegFormat
                    }
                    |> createImageAddress
                    |> Expect.equal "https://example.org/iiif/2/abc/full/max/0/default.jpg"
            )
        , test "createImageAddress normalizes StaticImageUri boundaries without changing query slashes"
            (\_ ->
                StaticImageUri
                    { host = "https://example.org/"
                    , prefix = "//images/example.jpg?redirect=https://other.example/"
                    }
                    |> createImageAddress
                    |> Expect.equal "https://example.org/images/example.jpg?redirect=https://other.example/"
            )
        , test "parseImageAddress still round-trips a static URL with a query string"
            (\_ ->
                parseImageAddress "https://example.org/images/example.jpg?download=1"
                    |> Maybe.map createImageAddress
                    |> Expect.equal (Just "https://example.org/images/example.jpg?download=1")
            )
        ]
