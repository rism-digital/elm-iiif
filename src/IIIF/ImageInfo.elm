module IIIF.ImageInfo exposing (ComplianceLevel(..), IIIFInfo(..), InfoJson, InfoProfile, WidthHeight, WidthHeightScale, toInfoJson)

{-| Types and helpers for IIIF Image API `info.json`.

@docs ComplianceLevel, IIIFInfo, InfoJson, InfoProfile, WidthHeight, WidthHeightScale, toInfoJson

-}

import IIIF.Image exposing (ImageUri)
import IIIF.Version exposing (IIIFVersion)


{-| A version-tagged info.json wrapper.
-}
type IIIFInfo
    = IIIFInfo IIIFVersion InfoJson


{-| The decoded info.json record.
-}
type alias InfoJson =
    { id : ImageUri
    , width : Int
    , height : Int
    , sizes : Maybe (List WidthHeight)
    , tiles : Maybe (List WidthHeightScale)
    , profile : Maybe InfoProfile
    }


{-| The declared Image API compliance level.
-}
type ComplianceLevel
    = Level0
    | Level1
    | Level2
    | UnknownLevel String


{-| Profile metadata normalized across Image API v1.1, v2, and v3.
-}
type alias InfoProfile =
    { complianceLevel : ComplianceLevel
    , formats : Maybe (List String)
    , qualities : Maybe (List String)
    , supports : Maybe (List String)
    , maxWidth : Maybe Int
    , maxHeight : Maybe Int
    , maxArea : Maybe Int
    }


{-| Width/height pair.
-}
type alias WidthHeight =
    { width : Int
    , height : Int
    }


{-| Width/height plus scale factors.
-}
type alias WidthHeightScale =
    { width : Int
    , height : Maybe Int
    , scaleFactors : List Int
    }


{-| Extract the underlying info.json record.
-}
toInfoJson : IIIFInfo -> InfoJson
toInfoJson (IIIFInfo _ infoJson) =
    infoJson
