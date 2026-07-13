module IIIF.Auth exposing
    ( AccessProfile(..), TokenErrorProfile(..), RelatedService(..)
    , AccessToken, AccessService, ActiveAccessService, AccessTokenService, LogoutService
    , ProbeLocation, ProbeResult, ProbeService, TokenError
    , accessTokenDecoder, activeAccessServiceDecoder, accessTokenServiceDecoder
    , logoutServiceDecoder, probeResultDecoder, probeServiceDecoder, tokenErrorDecoder
    )

{-| Structured decoders for the IIIF Authorization Flow 2.0.

@docs AccessProfile, TokenErrorProfile, RelatedService
@docs AccessToken, AccessService, ActiveAccessService, AccessTokenService, LogoutService
@docs ProbeLocation, ProbeResult, ProbeService, TokenError
@docs accessTokenDecoder, activeAccessServiceDecoder, accessTokenServiceDecoder
@docs logoutServiceDecoder, probeResultDecoder, probeServiceDecoder, tokenErrorDecoder

-}

import IIIF.Internal.Utilities exposing (custom, optional, required)
import IIIF.Language exposing (LanguageMap, languageMapLabelDecoder)
import Json.Decode as Decode exposing (Decoder, Value, andThen, fail, field, int, list, map, map2, oneOf, string, succeed, value)


authContext : String
authContext =
    "http://iiif.io/api/auth/2/context.json"


type AccessProfile
    = Active
    | Kiosk
    | External


type TokenErrorProfile
    = InvalidRequest
    | InvalidOrigin
    | MissingAspect
    | InvalidAspect
    | ExpiredAspect
    | Unavailable


type RelatedService
    = RelatedTokenService AccessTokenService
    | RelatedLogoutService LogoutService


type alias ProbeService =
    { id : String
    , type_ : String
    , services : List AccessService
    , errorHeading : Maybe LanguageMap
    , errorNote : Maybe LanguageMap
    }


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


type alias AccessTokenService =
    { id : String
    , type_ : String
    , errorHeading : Maybe LanguageMap
    , errorNote : Maybe LanguageMap
    }


type alias LogoutService =
    { id : String
    , type_ : String
    , label : Maybe LanguageMap
    }


type alias AccessToken =
    { context : String
    , type_ : String
    , accessToken : String
    , expiresIn : Maybe Int
    , messageId : String
    }


type alias TokenError =
    { context : String
    , type_ : String
    , profile : TokenErrorProfile
    , heading : Maybe LanguageMap
    , note : Maybe LanguageMap
    , messageId : String
    }


type alias ProbeLocation =
    { id : String
    , type_ : String
    }


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
    exact authContext contextDecoder


authTypeDecoder : String -> Decoder String
authTypeDecoder expected =
    exact expected typeDecoder


objectOrList : Decoder a -> Decoder (List a)
objectOrList decoder =
    oneOf [ list decoder, map List.singleton decoder ]


optionalStrict : String -> Decoder a -> Decoder (Maybe a)
optionalStrict fieldName decoder =
    value
        |> andThen
            (\object ->
                case Decode.decodeValue (field fieldName value) object of
                    Err _ ->
                        succeed Nothing

                    Ok fieldValue ->
                        case Decode.decodeValue decoder fieldValue of
                            Ok decoded ->
                                succeed (Just decoded)

                            Err error ->
                                fail (Decode.errorToString error)
            )


accessProfileDecoder : Decoder AccessProfile
accessProfileDecoder =
    string
        |> andThen
            (\profile ->
                case profile of
                    "active" ->
                        succeed Active

                    "kiosk" ->
                        succeed Kiosk

                    "external" ->
                        succeed External

                    _ ->
                        fail ("Unsupported Auth 2 access profile: " ++ profile)
            )


tokenErrorProfileDecoder : Decoder TokenErrorProfile
tokenErrorProfileDecoder =
    string
        |> andThen
            (\profile ->
                case profile of
                    "invalidRequest" ->
                        succeed InvalidRequest

                    "invalidOrigin" ->
                        succeed InvalidOrigin

                    "missingAspect" ->
                        succeed MissingAspect

                    "invalidAspect" ->
                        succeed InvalidAspect

                    "expiredAspect" ->
                        succeed ExpiredAspect

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


probeServiceDecoder : Decoder ProbeService
probeServiceDecoder =
    succeed ProbeService
        |> custom idDecoder
        |> custom (authTypeDecoder "AuthProbeService2")
        |> required "service" (objectOrList activeAccessServiceDecoder)
        |> custom (optionalStrict "errorHeading" languageMapLabelDecoder)
        |> custom (optionalStrict "errorNote" languageMapLabelDecoder)


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
                    idDecoder |> map Just
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


accessTokenServiceDecoder : Decoder AccessTokenService
accessTokenServiceDecoder =
    succeed AccessTokenService
        |> custom idDecoder
        |> custom (authTypeDecoder "AuthAccessTokenService2")
        |> custom (optionalStrict "errorHeading" languageMapLabelDecoder)
        |> custom (optionalStrict "errorNote" languageMapLabelDecoder)


logoutServiceDecoder : Decoder LogoutService
logoutServiceDecoder =
    succeed LogoutService
        |> custom idDecoder
        |> custom (authTypeDecoder "AuthLogoutService2")
        |> custom (optionalStrict "label" languageMapLabelDecoder)


accessTokenDecoder : Decoder AccessToken
accessTokenDecoder =
    succeed AccessToken
        |> custom authContextDecoder
        |> custom (authTypeDecoder "AuthAccessToken2")
        |> required "accessToken" string
        |> custom (optionalStrict "expiresIn" positiveIntDecoder)
        |> required "messageId" string


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


probeResultDecoder : Decoder ProbeResult
probeResultDecoder =
    succeed ProbeResult
        |> custom authContextDecoder
        |> custom (authTypeDecoder "AuthProbeResult2")
        |> required "status" httpStatusDecoder
        |> custom (optionalStrict "substitute" (objectOrList value) |> map (Maybe.withDefault []))
        |> custom (optionalStrict "heading" languageMapLabelDecoder)
        |> custom (optionalStrict "note" languageMapLabelDecoder)
        |> custom (optionalStrict "location" probeLocationDecoder)


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
