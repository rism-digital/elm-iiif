module IIIF exposing
    ( requestManifest, requestInfo, requestResource
    , v2PresentationContext, v3PresentationContext, v2ImageContext, v3ImageContext
    )

{-| Give a URL to a IIIF Resource, fetch, and decode the result.

If you know that the URL will respond with a Manifest, then you can call `requestManifest`. Likewise, if you
know that the response will be an Image API info.json response, call `requestInfo`.

However, if you are unsure if the response will be a manifest, collection, canvas, or range (as you might be when traversing
a IIIF collection that has embedded collections or manifests) then call `requestResource`. This also supports dereferencing
canvas and range URIs.

Additionally, the JSON-LD context strings are provided for both v2 and v3 Image and Presentation APIs. These are used
internally to determine how to decode the response, but they may also be useful in consuming applications; for example, if
a server uses content negotiation to switch between a v2 or v3 API response, then you can use this string as the `profile=`
value. (See, for example: <https://iiif.io/api/image/3.0/#51-image-information-request>)

@docs requestManifest, requestInfo, requestResource
@docs v2PresentationContext, v3PresentationContext, v2ImageContext, v3ImageContext

-}

import Http exposing (Expect)
import IIIF.Decoders exposing (infoJsonDecoder, manifestDecoder, resourceDecoder)
import IIIF.ImageInfo exposing (IIIFInfo(..))
import IIIF.Internal.Contexts as Contexts
import IIIF.Internal.Request exposing (request)
import IIIF.Presentation exposing (IIIFManifest, IIIFResource)


{-| Request a manifest by URL.
-}
requestManifest :
    (Result Http.Error IIIFManifest -> msg)
    -> List String
    -> String
    -> Cmd msg
requestManifest responseMsg acceptHeaders manifest =
    request acceptHeaders (Http.expectJson responseMsg manifestDecoder) manifest


{-| Request a IIIF resource (manifest or collection) by URL.
-}
requestResource :
    (Result Http.Error IIIFResource -> msg)
    -> List String
    -> String
    -> Cmd msg
requestResource responseMsg acceptHeaders url =
    request acceptHeaders (Http.expectJson responseMsg resourceDecoder) url


{-| Request a IIIF info.json response by URL.
-}
requestInfo :
    (Result Http.Error IIIFInfo -> msg)
    -> List String
    -> String
    -> Cmd msg
requestInfo responseMsg acceptHeaders url =
    request acceptHeaders (Http.expectJson responseMsg infoJsonDecoder) url


{-| The IIIF v2 Presentation Context String. Useful for detecting a IIIF v2 Manifest
-}
v2PresentationContext : String
v2PresentationContext =
    Contexts.iiifV2PresentationContextString


{-| The IIIF v3 Presentation Context String. Useful for detecting a IIIF v3 Manifest
-}
v3PresentationContext : String
v3PresentationContext =
    Contexts.iiifV3PresentationContextString


{-| The IIIF v2 Image Info Context String. Useful for detecting a IIIF v2 Image Server Response
-}
v2ImageContext : String
v2ImageContext =
    Contexts.iiifV2ImageContextString


{-| The IIIF v3 Image Info Context String. Useful for detecting a IIIF v3 Image Server Response
-}
v3ImageContext : String
v3ImageContext =
    Contexts.iiifV3ImageContextString
