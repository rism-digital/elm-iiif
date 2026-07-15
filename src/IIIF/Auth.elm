module IIIF.Auth exposing
    ( AccessProfile(..), TokenErrorProfile(..), RelatedService(..)
    , AccessToken, AccessService, ActiveAccessService, AccessTokenService, LogoutService
    , AuthDiscovery, ProbeLocation, ProbeResult, ProbeService, TokenError
    , accessTokenDecoder, activeAccessServiceDecoder, accessTokenServiceDecoder
    , authServicesDecoder, logoutServiceDecoder, probeResultDecoder, probeServiceDecoder, tokenErrorDecoder
    )

{-| Structured decoders for the IIIF Authorization Flow 2.0.

See the [IIIF Authorization Flow API 2.0 specification](https://iiif.io/api/auth/2.0/)
for the service declarations, interaction profiles, and response formats modeled
by this module.

@docs AccessProfile, TokenErrorProfile, RelatedService
@docs AccessToken, AccessService, ActiveAccessService, AccessTokenService, LogoutService
@docs AuthDiscovery, ProbeLocation, ProbeResult, ProbeService, TokenError
@docs accessTokenDecoder, activeAccessServiceDecoder, accessTokenServiceDecoder
@docs authServicesDecoder, logoutServiceDecoder, probeResultDecoder, probeServiceDecoder, tokenErrorDecoder

-}

import IIIF.Internal.Contexts exposing (contextMatches)
import IIIF.Internal.Utilities exposing (custom, required)
import IIIF.Language exposing (LanguageMap, languageMapLabelDecoder)
import Json.Decode as Decode exposing (Decoder, Value, andThen, fail, field, int, list, map, map2, oneOf, string, succeed, value)


authContext : String
authContext =
    "http://iiif.io/api/auth/2/context.json"


{-| Interaction profile declared by an Auth 2 access service.

`Active` opens a user-driven sign-in interaction, `Kiosk` is intended for
automatic access at a known location, and `External` delegates access outside
the client.

-}
type AccessProfile
    = Active
    | Kiosk
    | External


{-| Standard error profile returned by an Auth 2 access-token service.
-}
type TokenErrorProfile
    = InvalidRequest
    | InvalidOrigin
    | MissingAspect
    | InvalidAspect
    | ExpiredAspect
    | Unavailable


{-| A service nested under an Auth 2 access service.

The Auth 2 specification requires one access-token service and permits one
logout service.

-}
type RelatedService
    = RelatedTokenService AccessTokenService
    | RelatedLogoutService LogoutService


{-| An Auth 2 probe service and its candidate access services.

The optional error fields contain language maps suitable for display when a
probe cannot authorize the requested resource.

-}
type alias ProbeService =
    { id : String
    , type_ : String
    , services : List AccessService
    , errorHeading : Maybe LanguageMap
    , errorNote : Maybe LanguageMap
    }


{-| Auth services discovered while recursively traversing a IIIF service tree.

Recognized Auth 2 probes are decoded into `probes`. Auth-prefixed service types
that this module does not implement, including Auth 1 services, are retained in
`unsupportedServiceTypes` so applications can report them explicitly.

-}
type alias AuthDiscovery =
    { probes : List ProbeService
    , unsupportedServiceTypes : List String
    }


{-| An Auth 2 access service normalized across active, kiosk, and external profiles.

External services may omit `id`; active and kiosk services require it. The
profile-specific display fields are preserved as language maps. Descriptive
field relationships are intentionally decoded liberally: notes are retained
without their corresponding headings, even where the Auth 2 specification
describes the heading as required when a note is present.

-}
type alias AccessService =
    { id : Maybe String
    , type_ : String
    , profile : AccessProfile
    , services : List RelatedService
    , label : Maybe LanguageMap
    , heading : Maybe LanguageMap
    , note : Maybe LanguageMap
    , confirmLabel : Maybe LanguageMap
    }


{-| Compatibility name for an Auth access service. Despite the historical name,
this record can represent all three Auth 2 access profiles.
-}
type alias ActiveAccessService =
    AccessService


{-| The token service associated with an Auth 2 access service.
-}
type alias AccessTokenService =
    { id : String
    , type_ : String
    , errorHeading : Maybe LanguageMap
    , errorNote : Maybe LanguageMap
    }


{-| An optional Auth 2 logout service.
-}
type alias LogoutService =
    { id : String
    , type_ : String
    , label : Maybe LanguageMap
    }


{-| A successful Auth 2 access-token response.

`expiresIn` is measured in seconds when present. `messageId` correlates the
response with the hidden token-frame request that initiated it.

-}
type alias AccessToken =
    { context : String
    , type_ : String
    , accessToken : String
    , expiresIn : Maybe Int
    , messageId : String
    }


{-| A structured error returned by an Auth 2 access-token service.
-}
type alias TokenError =
    { context : String
    , type_ : String
    , profile : TokenErrorProfile
    , heading : Maybe LanguageMap
    , note : Maybe LanguageMap
    , messageId : String
    }


{-| A resource location supplied by a redirecting probe response.
-}
type alias ProbeLocation =
    { id : String
    , type_ : String
    }


{-| The authorization status returned by an Auth 2 probe.

Redirect locations and substitute resources are preserved for clients that
support those flows, even when they choose not to follow them automatically.

-}
type alias ProbeResult =
    { context : String
    , type_ : String
    , status : Int
    , substitutes : List Value
    , heading : Maybe LanguageMap
    , note : Maybe LanguageMap
    , location : Maybe ProbeLocation
    }


idDecoder : Decoder String
idDecoder =
    oneOf [ field "id" string, field "@id" string ]


httpsIdDecoder : Decoder String
httpsIdDecoder =
    idDecoder
        |> andThen
            (\id ->
                if String.startsWith "https://" id then
                    succeed id

                else
                    fail "Expected an HTTPS service identifier"
            )


typeDecoder : Decoder String
typeDecoder =
    oneOf [ field "type" string, field "@type" string ]


contextDecoder : Decoder String
contextDecoder =
    oneOf [ field "@context" string, field "context" string ]


exact : String -> Decoder String -> Decoder String
exact expected decoder =
    decoder
        |> andThen
            (\actual ->
                if actual == expected then
                    succeed actual

                else
                    fail ("Expected " ++ expected ++ " but got " ++ actual)
            )


authContextDecoder : Decoder String
authContextDecoder =
    contextDecoder
        |> andThen
            (\actual ->
                if contextMatches authContext actual then
                    succeed actual

                else
                    fail ("Expected " ++ authContext ++ " but got " ++ actual)
            )


authTypeDecoder : String -> Decoder String
authTypeDecoder expected =
    exact expected typeDecoder


objectOrList : Decoder a -> Decoder (List a)
objectOrList decoder =
    oneOf [ list decoder, map List.singleton decoder ]


{-| Recursively discover Auth services in an arbitrary IIIF service object or list.

Malformed recognized Auth 2 declarations fail decoding. Unknown non-Auth
services are traversed, while unsupported Auth-prefixed types are reported in
the resulting `AuthDiscovery`.

-}
authServicesDecoder : Decoder AuthDiscovery
authServicesDecoder =
    value
        |> andThen
            (\root ->
                case discoverAuthServices root of
                    Ok discovery ->
                        succeed discovery

                    Err error ->
                        fail (Decode.errorToString error)
            )


discoverAuthServices : Value -> Result Decode.Error AuthDiscovery
discoverAuthServices root =
    let
        discover pending reversedProbes reversedUnsupported =
            case pending of
                [] ->
                    Ok
                        { probes = List.reverse reversedProbes
                        , unsupportedServiceTypes = List.reverse reversedUnsupported
                        }

                current :: remaining ->
                    case Decode.decodeValue (list value) current of
                        Ok values ->
                            discover (values ++ remaining) reversedProbes reversedUnsupported

                        Err _ ->
                            case Decode.decodeValue typeDecoder current of
                                Ok "AuthProbeService2" ->
                                    case Decode.decodeValue probeServiceDecoder current of
                                        Ok probe ->
                                            discover remaining (probe :: reversedProbes) reversedUnsupported

                                        Err error ->
                                            Err error

                                Ok serviceType ->
                                    let
                                        nextUnsupported =
                                            if String.startsWith "Auth" serviceType then
                                                serviceType :: reversedUnsupported

                                            else
                                                reversedUnsupported
                                    in
                                    case Decode.decodeValue (field "service" (objectOrList value)) current of
                                        Ok children ->
                                            discover (children ++ remaining) reversedProbes nextUnsupported

                                        Err _ ->
                                            discover remaining reversedProbes nextUnsupported

                                Err _ ->
                                    case Decode.decodeValue (field "service" (objectOrList value)) current of
                                        Ok children ->
                                            discover (children ++ remaining) reversedProbes reversedUnsupported

                                        Err _ ->
                                            discover remaining reversedProbes reversedUnsupported
    in
    discover [ root ] [] []


optionalStrict : String -> Decoder a -> Decoder (Maybe a)
optionalStrict fieldName decoder =
    value
        |> andThen
            (\object ->
                case Decode.decodeValue (field fieldName value) object of
                    Ok fieldValue ->
                        case Decode.decodeValue decoder fieldValue of
                            Ok decoded ->
                                succeed (Just decoded)

                            Err error ->
                                fail (Decode.errorToString error)

                    Err _ ->
                        succeed Nothing
            )


accessProfileDecoder : Decoder AccessProfile
accessProfileDecoder =
    string
        |> andThen
            (\profile ->
                case profile of
                    "active" ->
                        succeed Active

                    "external" ->
                        succeed External

                    "kiosk" ->
                        succeed Kiosk

                    _ ->
                        fail ("Unsupported Auth 2 access profile: " ++ profile)
            )


tokenErrorProfileDecoder : Decoder TokenErrorProfile
tokenErrorProfileDecoder =
    string
        |> andThen
            (\profile ->
                case profile of
                    "expiredAspect" ->
                        succeed ExpiredAspect

                    "invalidAspect" ->
                        succeed InvalidAspect

                    "invalidOrigin" ->
                        succeed InvalidOrigin

                    "invalidRequest" ->
                        succeed InvalidRequest

                    "missingAspect" ->
                        succeed MissingAspect

                    "unavailable" ->
                        succeed Unavailable

                    _ ->
                        fail ("Unsupported Auth 2 token error profile: " ++ profile)
            )


positiveIntDecoder : Decoder Int
positiveIntDecoder =
    int
        |> andThen
            (\number ->
                if number > 0 then
                    succeed number

                else
                    fail "Expected a positive integer"
            )


{-| Decode an `AuthProbeService2`, accepting either v2 (`@id`, `@type`) or v3
(`id`, `type`) property names and object-or-array service nesting.
-}
probeServiceDecoder : Decoder ProbeService
probeServiceDecoder =
    succeed ProbeService
        |> custom httpsIdDecoder
        |> custom (authTypeDecoder "AuthProbeService2")
        |> required "service" (objectOrList activeAccessServiceDecoder)
        |> custom (optionalStrict "errorHeading" languageMapLabelDecoder)
        |> custom (optionalStrict "errorNote" languageMapLabelDecoder)


{-| Decode an Auth 2 access service.

The compatibility name is historical: the decoder accepts active, kiosk, and
external profiles. It enforces their identifier and label requirements and
requires exactly one nested token service.

-}
activeAccessServiceDecoder : Decoder ActiveAccessService
activeAccessServiceDecoder =
    field "profile" accessProfileDecoder
        |> andThen activeAccessServiceForProfileDecoder


activeAccessServiceForProfileDecoder : AccessProfile -> Decoder ActiveAccessService
activeAccessServiceForProfileDecoder profile =
    succeed AccessService
        |> custom
            (case profile of
                External ->
                    succeed Nothing

                _ ->
                    httpsIdDecoder |> map Just
            )
        |> custom (authTypeDecoder "AuthAccessService2")
        |> custom (succeed profile)
        |> required "service" relatedServicesDecoder
        |> custom
            (case profile of
                Active ->
                    field "label" languageMapLabelDecoder |> map Just

                _ ->
                    optionalStrict "label" languageMapLabelDecoder
            )
        |> custom (optionalStrict "heading" languageMapLabelDecoder)
        |> custom (optionalStrict "note" languageMapLabelDecoder)
        |> custom (optionalStrict "confirmLabel" languageMapLabelDecoder)


relatedServiceDecoder : Decoder RelatedService
relatedServiceDecoder =
    typeDecoder
        |> andThen
            (\serviceType ->
                case serviceType of
                    "AuthAccessTokenService2" ->
                        map RelatedTokenService accessTokenServiceDecoder

                    "AuthLogoutService2" ->
                        map RelatedLogoutService logoutServiceDecoder

                    _ ->
                        fail ("Unsupported Auth 2 related service: " ++ serviceType)
            )


relatedServicesDecoder : Decoder (List RelatedService)
relatedServicesDecoder =
    objectOrList relatedServiceDecoder
        |> andThen
            (\services ->
                let
                    tokenCount =
                        List.filter
                            (\service ->
                                case service of
                                    RelatedTokenService _ ->
                                        True

                                    RelatedLogoutService _ ->
                                        False
                            )
                            services
                            |> List.length

                    logoutCount =
                        List.length services - tokenCount
                in
                if tokenCount == 1 && logoutCount <= 1 then
                    succeed services

                else
                    fail "Auth access services require exactly one token service and at most one logout service"
            )


{-| Decode an `AuthAccessTokenService2`, including optional localized error text.
-}
accessTokenServiceDecoder : Decoder AccessTokenService
accessTokenServiceDecoder =
    succeed AccessTokenService
        |> custom httpsIdDecoder
        |> custom (authTypeDecoder "AuthAccessTokenService2")
        |> custom (optionalStrict "errorHeading" languageMapLabelDecoder)
        |> custom (optionalStrict "errorNote" languageMapLabelDecoder)


{-| Decode an `AuthLogoutService2` and its optional localized label.
-}
logoutServiceDecoder : Decoder LogoutService
logoutServiceDecoder =
    succeed LogoutService
        |> custom httpsIdDecoder
        |> custom (authTypeDecoder "AuthLogoutService2")
        |> custom (optionalStrict "label" languageMapLabelDecoder)


{-| Decode a successful `AuthAccessToken2` response.

Both HTTP and HTTPS forms of the official Auth 2 context are accepted. When
present, `expiresIn` must be a positive integer.

-}
accessTokenDecoder : Decoder AccessToken
accessTokenDecoder =
    succeed AccessToken
        |> custom authContextDecoder
        |> custom (authTypeDecoder "AuthAccessToken2")
        |> required "accessToken" string
        |> custom (optionalStrict "expiresIn" positiveIntDecoder)
        |> required "messageId" string


{-| Decode an `AuthAccessTokenError2` response and its standard error profile.
-}
tokenErrorDecoder : Decoder TokenError
tokenErrorDecoder =
    succeed TokenError
        |> custom authContextDecoder
        |> custom (authTypeDecoder "AuthAccessTokenError2")
        |> required "profile" tokenErrorProfileDecoder
        |> custom (optionalStrict "heading" languageMapLabelDecoder)
        |> custom (optionalStrict "note" languageMapLabelDecoder)
        |> required "messageId" string


probeLocationDecoder : Decoder ProbeLocation
probeLocationDecoder =
    map2 ProbeLocation idDecoder typeDecoder


{-| Decode an `AuthProbeResult2` response.

HTTP status values, localized display text, redirect locations, and substitute
resources are preserved.

-}
probeResultDecoder : Decoder ProbeResult
probeResultDecoder =
    field "status" httpStatusDecoder
        |> andThen probeResultForStatusDecoder


probeResultForStatusDecoder : Int -> Decoder ProbeResult
probeResultForStatusDecoder status =
    succeed ProbeResult
        |> custom authContextDecoder
        |> custom (authTypeDecoder "AuthProbeResult2")
        |> custom (succeed status)
        |> custom
            (statusDependentOptional
                "substitute"
                (status == 401 || status == 403)
                (objectOrList value)
                |> map (Maybe.withDefault [])
            )
        |> custom (optionalStrict "heading" languageMapLabelDecoder)
        |> custom (optionalStrict "note" languageMapLabelDecoder)
        |> custom
            (statusDependentOptional
                "location"
                (status >= 300 && status <= 399)
                probeLocationDecoder
            )


statusDependentOptional : String -> Bool -> Decoder a -> Decoder (Maybe a)
statusDependentOptional fieldName allowed decoder =
    value
        |> andThen
            (\object ->
                case Decode.decodeValue (field fieldName value) object of
                    Ok fieldValue ->
                        if allowed then
                            case Decode.decodeValue decoder fieldValue of
                                Ok decoded ->
                                    succeed (Just decoded)

                                Err error ->
                                    fail (Decode.errorToString error)

                        else
                            fail (fieldName ++ " is not permitted for this probe status")

                    Err _ ->
                        succeed Nothing
            )


httpStatusDecoder : Decoder Int
httpStatusDecoder =
    int
        |> andThen
            (\status ->
                if status >= 100 && status <= 599 then
                    succeed status

                else
                    fail "Expected an HTTP status code between 100 and 599"
            )
