module IIIF.Internal.Utilities exposing
    ( applyMaybe
    , cartesianProduct
    , custom
    , find
    , hardcoded
    , initialize
    , oneOfMaybes
    , optional
    , remove
    , required
    , requiredAt
    )

import Json.Decode as Decode exposing (Decoder)


applyMaybe : Maybe a -> Maybe (a -> b) -> Maybe b
applyMaybe argParser funcParser =
    Maybe.map2 (<|) funcParser argParser


applyDecoder : Decoder a -> Decoder (a -> b) -> Decoder b
applyDecoder valueDecoder pipeline =
    Decode.map2 (\value fn -> fn value) valueDecoder pipeline


oneOfMaybes : List (a -> Maybe b) -> a -> Maybe b
oneOfMaybes fmbs a =
    case fmbs of
        [] ->
            Nothing

        fmb :: rest ->
            case fmb a of
                Just b ->
                    Just b

                Nothing ->
                    oneOfMaybes rest a


required : String -> Decoder a -> Decoder (a -> b) -> Decoder b
required field decoder pipeline =
    applyDecoder (Decode.field field decoder) pipeline


optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional field decoder fallback pipeline =
    let
        fieldDecoder =
            Decode.oneOf
                [ Decode.field field decoder
                , Decode.succeed fallback
                ]
    in
    applyDecoder fieldDecoder pipeline


requiredAt : List String -> Decoder a -> Decoder (a -> b) -> Decoder b
requiredAt path decoder pipeline =
    applyDecoder (Decode.at path decoder) pipeline


optionalAt : List String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optionalAt path decoder fallback pipeline =
    applyDecoder
        (Decode.oneOf
            [ Decode.at path decoder
            , Decode.succeed fallback
            ]
        )
        pipeline


hardcoded : a -> Decoder (a -> b) -> Decoder b
hardcoded value pipeline =
    applyDecoder (Decode.succeed value) pipeline


custom : Decoder a -> Decoder (a -> b) -> Decoder b
custom decoder pipeline =
    applyDecoder decoder pipeline


initialize : Int -> (Int -> a) -> List a
initialize count mapper =
    if count <= 0 then
        []

    else
        List.range 0 (count - 1)
            |> List.map mapper


remove : a -> List a -> List a
remove item items =
    List.filter ((/=) item) items


cartesianProduct : List (List a) -> List (List a)
cartesianProduct lists =
    List.foldr
        (\current acc ->
            List.concatMap (\item -> List.map ((::) item) acc) current
        )
        [ [] ]
        lists


find : (a -> Bool) -> List a -> Maybe a
find predicate items =
    case items of
        [] ->
            Nothing

        x :: rest ->
            if predicate x then
                Just x

            else
                find predicate rest
