module IIIF.Internal.Contexts exposing (contextMatches, iiifV2ImageContextString, iiifV2PresentationContextString, iiifV3ImageContextString, iiifV3PresentationContextString, isV1ImageContext)

{-| Constants representing known IIIF Context Values
-}


contextMatches : String -> String -> Bool
contextMatches expected actual =
    -- Some manifests use the HTTPS version of the context string which is not compatible with the spec
    -- but we can also handle that since it's mostly harmless.
    actual == expected || actual == String.replace "http://" "https://" expected


{-| Whether a context identifies a legacy IIIF Image API 1 service or
Image API 1.1 information document.
-}
isV1ImageContext : String -> Bool
isV1ImageContext actual =
    contextMatches iiifV1ImageContextString actual
        || contextMatches iiifV1ImageServiceContextString actual


{-| IIIF Image API v1.1 information context URL.
-}
iiifV1ImageContextString : String
iiifV1ImageContextString =
    "http://library.stanford.edu/iiif/image-api/1.1/context.json"


{-| Legacy Image API v1 context used in Presentation API service declarations.
-}
iiifV1ImageServiceContextString : String
iiifV1ImageServiceContextString =
    "http://iiif.io/api/image/1/context.json"


{-| IIIF Presentation v3 context URL. This will always return
`http://iiif.io/api/presentation/3/context.json`.
-}
iiifV3PresentationContextString : String
iiifV3PresentationContextString =
    "http://iiif.io/api/presentation/3/context.json"


{-| IIIF Presentation v2 context URL. This will always return
`http://iiif.io/api/presentation/2/context.json`
-}
iiifV2PresentationContextString : String
iiifV2PresentationContextString =
    "http://iiif.io/api/presentation/2/context.json"


{-| IIIF Image API v3 context URL. This will always return
`http://iiif.io/api/image/3/context.json`
-}
iiifV3ImageContextString : String
iiifV3ImageContextString =
    "http://iiif.io/api/image/3/context.json"


{-| IIIF Image API v2 context URL. This will always return
`http://iiif.io/api/image/2/context.json`
-}
iiifV2ImageContextString : String
iiifV2ImageContextString =
    "http://iiif.io/api/image/2/context.json"
