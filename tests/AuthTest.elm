module AuthTest exposing (tests)

import Expect
import IIIF.Auth exposing (AccessProfile(..), RelatedService(..), TokenErrorProfile(..), accessTokenDecoder, activeAccessServiceDecoder, probeResultDecoder, probeServiceDecoder, tokenErrorDecoder)
import IIIF.Decoders exposing (manifestDecoder)
import IIIF.Language exposing (Language(..), LanguageValues(..))
import IIIF.Presentation exposing (IIIFManifest(..))
import Json.Decode as Decode exposing (Decoder)
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Auth"
        [ test "manifestDecoder preserves v3 Auth 2 services on image bodies"
            (\_ ->
                expectManifestImageServiceObjects v3ManifestWithAuthService [ "AuthProbeService2" ]
            )
        , test "manifestDecoder preserves v2 Auth 2 services on image resources"
            (\_ ->
                expectManifestImageServiceObjects v2ManifestWithAuthService [ "AuthProbeService2" ]
            )
        , test "activeAccessServiceDecoder accepts active profile with recommended display strings omitted"
            (\_ ->
                case Decode.decodeString activeAccessServiceDecoder activeAccessWithoutDisplayStrings of
                    Ok service ->
                        Expect.equal True
                            (service.id
                                == Just "https://auth.example.org/login"
                                && service.profile
                                == Active
                                && List.length service.services
                                == 1
                                && service.label
                                == Just [ LanguageValues (LanguageCode "en") [ "Sign in" ] ]
                                && service.heading
                                == Nothing
                                && service.note
                                == Nothing
                                && service.confirmLabel
                                == Nothing
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "activeAccessServiceDecoder accepts kiosk profiles"
            (\_ ->
                case Decode.decodeString activeAccessServiceDecoder kioskAccessService of
                    Ok service ->
                        Expect.equal Kiosk service.profile

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "activeAccessServiceDecoder accepts external profiles without an id"
            (\_ ->
                case Decode.decodeString activeAccessServiceDecoder externalAccessService of
                    Ok service ->
                        Expect.equal ( External, Nothing ) ( service.profile, service.id )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "activeAccessServiceDecoder requires a label for the active profile"
            (\_ ->
                case Decode.decodeString activeAccessServiceDecoder activeAccessWithoutLabel of
                    Ok _ ->
                        Expect.fail "Expected an active service without a label to fail"

                    Err _ ->
                        Expect.pass
            )
        , test "activeAccessServiceDecoder requires exactly one token service"
            (\_ ->
                case Decode.decodeString activeAccessServiceDecoder activeAccessWithoutToken of
                    Ok _ ->
                        Expect.fail "Expected an access service without a token service to fail"

                    Err _ ->
                        Expect.pass
            )
        , test "probeServiceDecoder traverses access and token services"
            (\_ ->
                case Decode.decodeString probeServiceDecoder probeService of
                    Ok service ->
                        case service.services of
                            [ accessService ] ->
                                case accessService.services of
                                    [ RelatedTokenService tokenService ] ->
                                        Expect.equal "https://auth.example.org/token" tokenService.id

                                    _ ->
                                        Expect.fail "Expected one token service"

                            _ ->
                                Expect.fail "Expected one access service"

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "accessTokenDecoder parses token responses"
            (\_ ->
                case Decode.decodeString accessTokenDecoder tokenResponse of
                    Ok token ->
                        Expect.equal True
                            (token.accessToken
                                == "abc123"
                                && token.expiresIn
                                == Just 300
                                && token.messageId
                                == "msg-1"
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "tokenErrorDecoder parses Auth 2 token errors"
            (\_ ->
                case Decode.decodeString tokenErrorDecoder tokenErrorResponse of
                    Ok err ->
                        Expect.equal True
                            (err.profile
                                == MissingAspect
                                && err.messageId
                                == "msg-1"
                                && err.heading
                                == Just [ LanguageValues (LanguageCode "en") [ "Unauthorized" ] ]
                                && err.note
                                == Just [ LanguageValues (LanguageCode "en") [ "No access cookie was present" ] ]
                            )

                    Err decodeErr ->
                        Expect.fail (Decode.errorToString decodeErr)
            )
        , test "probeResultDecoder parses failure responses with display text"
            (\_ ->
                case Decode.decodeString probeResultDecoder probeFailureResponse of
                    Ok result ->
                        Expect.equal True
                            (result.status
                                == 401
                                && result.heading
                                == Just [ LanguageValues (LanguageCode "en") [ "Access denied" ] ]
                                && result.note
                                == Just [ LanguageValues (LanguageCode "en") [ "Sign in first" ] ]
                            )

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "probeResultDecoder parses redirect location objects"
            (\_ ->
                case Decode.decodeString probeResultDecoder probeRedirectResponse of
                    Ok result ->
                        Expect.equal (Just { id = "https://auth.example.org/redirected.jpg", type_ = "Image" }) result.location

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "probeResultDecoder preserves substitute resources"
            (\_ ->
                case Decode.decodeString probeResultDecoder probeSubstituteResponse of
                    Ok result ->
                        Expect.equal 1 (List.length result.substitutes)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
            )
        , test "accessTokenDecoder rejects non-positive expiry values"
            (\_ ->
                case Decode.decodeString accessTokenDecoder tokenResponseWithZeroExpiry of
                    Ok _ ->
                        Expect.fail "Expected zero expiresIn to fail"

                    Err _ ->
                        Expect.pass
            )
        ]


expectManifestImageServiceObjects : String -> List String -> Expect.Expectation
expectManifestImageServiceObjects json expectedTypes =
    case Decode.decodeString manifestDecoder json of
        Ok (IIIFManifest _ manifest) ->
            case List.head manifest.canvases |> Maybe.andThen (.images >> List.head) of
                Just image ->
                    case image.serviceObjects of
                        [] ->
                            Expect.fail "Expected preserved service objects"

                        rawService :: _ ->
                            case Decode.decodeValue nestedServiceTypesDecoder rawService of
                                Ok actualTypes ->
                                    Expect.equal expectedTypes actualTypes

                                Err err ->
                                    Expect.fail (Decode.errorToString err)

                Nothing ->
                    Expect.fail "Expected one image"

        Err err ->
            Expect.fail (Decode.errorToString err)


nestedServiceTypesDecoder : Decoder (List String)
nestedServiceTypesDecoder =
    Decode.field "service"
        (Decode.oneOf
            [ Decode.list serviceTypeDecoder
            , Decode.map List.singleton serviceTypeDecoder
            ]
        )


serviceTypeDecoder : Decoder String
serviceTypeDecoder =
    Decode.oneOf
        [ Decode.field "type" Decode.string
        , Decode.field "@type" Decode.string
        ]


activeAccessWithoutDisplayStrings : String
activeAccessWithoutDisplayStrings =
    """{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"active","label":{"en":["Sign in"]},"service":[{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}]}"""


kioskAccessService : String
kioskAccessService =
    """{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"kiosk","service":{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}}"""


externalAccessService : String
externalAccessService =
    """{"type":"AuthAccessService2","profile":"external","service":{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}}"""


activeAccessWithoutLabel : String
activeAccessWithoutLabel =
    """{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"active","service":{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}}"""


activeAccessWithoutToken : String
activeAccessWithoutToken =
    """{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"active","label":{"en":["Sign in"]},"service":{"id":"https://auth.example.org/logout","type":"AuthLogoutService2"}}"""


probeService : String
probeService =
    """{"id":"https://auth.example.org/probe","type":"AuthProbeService2","service":[{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"active","label":{"en":["Sign in"]},"service":[{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}]}]}"""


tokenResponse : String
tokenResponse =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthAccessToken2","accessToken":"abc123","expiresIn":300,"messageId":"msg-1"}"""


tokenResponseWithZeroExpiry : String
tokenResponseWithZeroExpiry =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthAccessToken2","accessToken":"abc123","expiresIn":0,"messageId":"msg-1"}"""


tokenErrorResponse : String
tokenErrorResponse =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthAccessTokenError2","profile":"missingAspect","heading":{"en":["Unauthorized"]},"note":{"en":["No access cookie was present"]},"messageId":"msg-1"}"""


probeFailureResponse : String
probeFailureResponse =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthProbeResult2","status":401,"heading":{"en":["Access denied"]},"note":{"en":["Sign in first"]}}"""


probeRedirectResponse : String
probeRedirectResponse =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthProbeResult2","status":302,"location":{"id":"https://auth.example.org/redirected.jpg","type":"Image"}}"""


probeSubstituteResponse : String
probeSubstituteResponse =
    """{"@context":"http://iiif.io/api/auth/2/context.json","type":"AuthProbeResult2","status":401,"substitute":[{"id":"https://example.org/watermarked.jpg","type":"Image"}]}"""


v3ManifestWithAuthService : String
v3ManifestWithAuthService =
    """{"@context":["http://iiif.io/api/auth/2/context.json","http://iiif.io/api/presentation/3/context.json"],"id":"https://example.org/manifest","type":"Manifest","label":{"en":["Auth Manifest"]},"items":[{"id":"https://example.org/canvas/1","type":"Canvas","width":100,"height":200,"items":[{"id":"https://example.org/page/1","type":"AnnotationPage","items":[{"id":"https://example.org/anno/1","type":"Annotation","motivation":"painting","target":"https://example.org/canvas/1","body":{"id":"https://example.org/image/full/full/0/default.jpg","type":"Image","service":{"id":"https://example.org/iiif/image","type":"ImageService3","service":[{"id":"https://auth.example.org/probe","type":"AuthProbeService2","service":[{"id":"https://auth.example.org/login","type":"AuthAccessService2","profile":"active","service":[{"id":"https://auth.example.org/token","type":"AuthAccessTokenService2"}]}]}]}}}]}]}]}"""


v2ManifestWithAuthService : String
v2ManifestWithAuthService =
    """{"@context":"http://iiif.io/api/presentation/2/context.json","@id":"https://example.org/manifest","@type":"sc:Manifest","label":"Auth Manifest","sequences":[{"@id":"https://example.org/seq/1","@type":"sc:Sequence","canvases":[{"@id":"https://example.org/canvas/1","@type":"sc:Canvas","width":100,"height":200,"images":[{"@id":"https://example.org/anno/1","@type":"oa:Annotation","motivation":"sc:painting","on":"https://example.org/canvas/1","resource":{"@id":"https://example.org/image/full/full/0/default.jpg","@type":"dctypes:Image","format":"image/jpeg","service":{"@id":"https://example.org/iiif/image","@context":"http://iiif.io/api/image/2/context.json","profile":"http://iiif.io/api/image/2/level1.json","service":[{"@id":"https://auth.example.org/probe","@type":"AuthProbeService2","service":[{"@id":"https://auth.example.org/login","@type":"AuthAccessService2","profile":"active","service":[{"@id":"https://auth.example.org/token","@type":"AuthAccessTokenService2"}]}]}]}}}]}]}]}"""
