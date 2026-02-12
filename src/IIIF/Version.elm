module IIIF.Version exposing (IIIFVersion(..))

{-| The IIIF API version in use.

Example:

    case version of
        IIIFV2 ->
            "IIIF v2"

        IIIFV3 ->
            "IIIF v3"

@docs IIIFVersion

-}


{-| IIIF API version tag.
-}
type IIIFVersion
    = IIIFV2
    | IIIFV3
