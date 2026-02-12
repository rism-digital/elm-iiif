module IIIF.ImageInfo exposing (IIIFInfo(..), InfoJson, InfoProfile, WidthHeight, WidthHeightScale, toInfoJson)

{-| Types and helpers for IIIF Image API `info.json`.

@docs IIIFInfo, InfoJson, InfoProfile, WidthHeight, WidthHeightScale, toInfoJson

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

    --, profile : InfoProfile
    }


{-| Optional profile metadata from the Image API.
-}
type alias InfoProfile =
    { formats : Maybe (List String)
    , qualities : Maybe (List String)
    , supports : Maybe (List String)
    , maxWidth : Maybe Int
    , maxHeight : Maybe Int
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
