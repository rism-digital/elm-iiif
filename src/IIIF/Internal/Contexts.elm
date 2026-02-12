module IIIF.Internal.Contexts exposing (iiifV2ImageContextString, iiifV2PresentationContextString, iiifV3ImageContextString, iiifV3PresentationContextString)

{-| Constants representing known IIIF Context Values
-}


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
