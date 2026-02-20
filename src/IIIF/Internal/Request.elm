module IIIF.Internal.Request exposing (request)

import Http exposing (Expect)


{-| Low-level request helper used by the other request functions.
-}
request : List String -> Expect msg -> String -> Cmd msg
request acceptHeaders expect url =
    Http.request
        { body = Http.emptyBody
        , expect = expect
        , headers = List.map (Http.header "Accept") acceptHeaders
        , method = "GET"
        , timeout = Nothing
        , tracker = Nothing
        , url = url
        }
