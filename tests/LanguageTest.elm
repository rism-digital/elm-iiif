module LanguageTest exposing (tests)

import Expect
import IIIF.Language exposing (Language(..), LanguageValues(..), extractLabelFromLanguageMap, languageMapLabelDecoder, stringToLanguageMapLabelDecoder, v2LanguageMapLabelDecoder)
import Json.Decode as Decode
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "IIIF.Language"
        [ test "extractLabelFromLanguageMap prefers requested language" <|
            \_ ->
                let
                    langMap =
                        [ LanguageValues (LanguageCode "en") [ "Hello", "World" ]
                        , LanguageValues None [ "None" ]
                        , LanguageValues Default [ "Default" ]
                        ]
                in
                extractLabelFromLanguageMap (LanguageCode "en") langMap
                    |> Expect.equal "Hello; World"
        , test "extractLabelFromLanguageMap falls back to none" <|
            \_ ->
                let
                    langMap =
                        [ LanguageValues None [ "None" ]
                        , LanguageValues Default [ "Default" ]
                        ]
                in
                extractLabelFromLanguageMap (LanguageCode "de") langMap
                    |> Expect.equal "None"
        , test "extractLabelFromLanguageMap falls back to default" <|
            \_ ->
                let
                    langMap =
                        [ LanguageValues Default [ "Default" ]
                        ]
                in
                extractLabelFromLanguageMap (LanguageCode "de") langMap
                    |> Expect.equal "Default"
        , test "stringToLanguageMapLabelDecoder wraps string as Default" <|
            \_ ->
                Decode.decodeString stringToLanguageMapLabelDecoder "\"Plain\""
                    |> Expect.equal (Ok [ LanguageValues Default [ "Plain" ] ])
        , test "v2LanguageMapLabelDecoder parses @value/@language object" <|
            \_ ->
                let
                    json =
                        "{\"@value\":\"Title\",\"@language\":\"fr\"}"
                in
                Decode.decodeString v2LanguageMapLabelDecoder json
                    |> Expect.equal (Ok [ LanguageValues (LanguageCode "fr") [ "Title" ] ])
        , test "v2LanguageMapLabelDecoder parses list of @value/@language objects" <|
            \_ ->
                let
                    json =
                        "[{\"@value\":\"Title\",\"@language\":\"en\"},{\"@value\":\"Titel\",\"@language\":\"de\"},{\"@value\":\"書名\",\"@language\":\"zh\"}]"
                in
                Decode.decodeString v2LanguageMapLabelDecoder json
                    |> Expect.equal
                        (Ok
                            [ LanguageValues (LanguageCode "en") [ "Title" ]
                            , LanguageValues (LanguageCode "de") [ "Titel" ]
                            , LanguageValues (LanguageCode "zh") [ "書名" ]
                            ]
                        )
        , test "languageMapLabelDecoder reads language map object" <|
            \_ ->
                let
                    json =
                        "{\"en\":[\"Hi\"],\"de\":[\"Hallo\"]}"

                    result =
                        Decode.decodeString languageMapLabelDecoder json
                in
                case result of
                    Ok langMap ->
                        let
                            hasEn =
                                List.any
                                    (\langValues ->
                                        case langValues of
                                            LanguageValues (LanguageCode "en") values ->
                                                values == [ "Hi" ]

                                            _ ->
                                                False
                                    )
                                    langMap

                            hasDe =
                                List.any
                                    (\langValues ->
                                        case langValues of
                                            LanguageValues (LanguageCode "de") values ->
                                                values == [ "Hallo" ]

                                            _ ->
                                                False
                                    )
                                    langMap
                        in
                        Expect.equal True (hasEn && hasDe)

                    Err err ->
                        Expect.fail (Decode.errorToString err)
        ]
