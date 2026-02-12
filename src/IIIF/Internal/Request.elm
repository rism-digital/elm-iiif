module IIIF.Internal.Request exposing (request)

import Http exposing (Expect)


{-| Low-level request helper used by the other request functions.
-}
request : List String -> Expect msg -> String -> Cmd msg
request acceptHeaders expect url =
    Http.request
        { method = "GET"
        , headers = List.map (Http.header "Accept") acceptHeaders
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
