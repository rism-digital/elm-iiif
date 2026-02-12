module IIIF.Validate exposing (validateManifest)

import IIIF.Decoders exposing (manifestDecoder)
import Json.Decode as Decode exposing (Value)


validateManifest : Value -> ( Bool, List String )
validateManifest json =
    case Decode.decodeValue manifestDecoder json of
        Ok _ ->
            ( True, [] )

        Err err ->
            ( False, [ Decode.errorToString err ] )
