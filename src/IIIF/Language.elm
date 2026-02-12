module IIIF.Language exposing
    ( LabelValue, Language(..), LanguageMap, LanguageValues(..)
    , extractLabelFromLanguageMap, labelValueDecoder, languageMapLabelDecoder
    , stringToLanguageMapLabelDecoder, v2LabelValueDecoder, v2LanguageMapLabelDecoder
    )

{-| Language and label helpers for IIIF values. IIIF v3 uses JSON-LD Language Maps
extensively, and this module helps decode these into strongly typed values.

IIIF v2 uses a different structure for multilingual content, but the decoders for v2 manifests
will detect and normalize these multilingual objects into a LanguageMap type.

@docs LabelValue, Language, LanguageMap, LanguageValues
@docs extractLabelFromLanguageMap, labelValueDecoder, languageMapLabelDecoder
@docs stringToLanguageMapLabelDecoder, v2LabelValueDecoder, v2LanguageMapLabelDecoder

-}

import IIIF.Internal.Utilities exposing (find, oneOfMaybes, required)
import Json.Decode as Decode exposing (Decoder, andThen, list, string)


{-| A language map holds one or more language-tagged values.
-}
type alias LanguageMap =
    List LanguageValues


{-| One language tag paired with its values.
-}
type LanguageValues
    = LanguageValues Language (List String)


{-| The encoded language. This can be helpful to try and map
a user's locale to a decoded language map.

If the language map has localizations, e.g., "en" or "de", then
it will be decoded as `LanguageCode "en"` or `LanguageCode "de"`.

If it has the special case value of "none" (used for non-localizable
strings in a language map) then it will be decoded as `None`.

If there is no language map (common with v2 manifests) then it will
be decoded as `Default`.

-}
type Language
    = LanguageCode String
    | None
    | Default


{-| A label/value pair, each expressed as a language map.

Example:

    { label = [ LanguageValues English [ "Title" ] ]
    , value = [ LanguageValues English [ "Some description" ] ]
    }

-}
type alias LabelValue =
    { label : LanguageMap
    , value : LanguageMap
    }


{-|

    Takes a string and returns the corresponding language type, e.g.,
    "en" -> English.

-}
parseLocaleToLanguage : String -> Language
parseLocaleToLanguage locale =
    if locale == "none" then
        None

    else
        LanguageCode locale


{-| Decoder for label/value pairs (v3 style).
-}
labelValueDecoder : Decoder LabelValue
labelValueDecoder =
    Decode.succeed LabelValue
        |> required "label" languageMapLabelDecoder
        |> required "value" languageMapLabelDecoder


{-| Decoder for label/value pairs (v2 style).
-}
v2LabelValueDecoder : Decoder LabelValue
v2LabelValueDecoder =
    Decode.succeed LabelValue
        |> required "label" v2LanguageMapLabelDecoder
        |> required "value" v2LanguageMapLabelDecoder


languageDecoder : String -> Decoder Language
languageDecoder locale =
    parseLocaleToLanguage locale
        |> Decode.succeed


languageValuesDecoder : ( String, List String ) -> Decoder LanguageValues
languageValuesDecoder ( locale, translations ) =
    languageDecoder locale
        |> Decode.map (\lang -> LanguageValues lang translations)


{-|

    A custom decoder that takes a JSON-LD Language Map and produces a list of
    LanguageValues Language (List String), representing each of the translations
    available for this particular field.

-}
languageMapDecoder : List ( String, List String ) -> Decoder LanguageMap
languageMapDecoder json =
    List.foldl
        (\map maps -> Decode.map2 (::) (languageValuesDecoder map) maps)
        (Decode.succeed [])
        json


{-| Decoder for a language map object.
-}
languageMapLabelDecoder : Decoder LanguageMap
languageMapLabelDecoder =
    Decode.keyValuePairs (list string)
        |> andThen languageMapDecoder


{-| Decoder for the case where an expected language map
is encoded as a string.
-}
stringToLanguageMapLabelDecoder : Decoder LanguageMap
stringToLanguageMapLabelDecoder =
    string
        |> andThen (\s -> Decode.succeed [ LanguageValues Default [ s ] ])


{-| Decoder for v2 language map variants.
-}
v2LanguageMapLabelDecoder : Decoder LanguageMap
v2LanguageMapLabelDecoder =
    Decode.oneOf
        [ v2LanguageValueObjectDecoder
        , stringToLanguageMapLabelDecoder
        , languageMapLabelDecoder
        ]


v2LanguageValueObjectDecoder : Decoder LanguageMap
v2LanguageValueObjectDecoder =
    Decode.map2
        (\value locale ->
            [ LanguageValues (parseLocaleToLanguage locale) [ value ] ]
        )
        (Decode.field "@value" string)
        (Decode.oneOf
            [ Decode.field "@language" string
            , Decode.succeed "none"
            ]
        )


{-| Extract a string from a language map given a language.

Use this function to present users with a localized view of the values in a manifest.

A language can be detected from the user's browser settings or a preference. Using this
detected value, and a LanguageMap object, this function will:

  - Choose the language string that matches their language; if none then
  - Choose the language string that matches anything marked with 'none'; if none then
  - Choose the first language string that matches the 'Default', which is likely the first language.
  - Choose the language string that matches the 'English', which is the most likely to occur
    in the widest number of manifests.

If none of those work, then it's likely the LanguageMap is somehow broken. This function will finally fall
back to returning "[No language value found]" as the string.

The 'value' of a LanguageMap is always a list. This function will present multiple values in the list
with a semicolon separator.

-}
extractLabelFromLanguageMap : Language -> LanguageMap -> String
extractLabelFromLanguageMap lang langMap =
    {-
       Returns a single string from a language map. Multiple values for a single language are
       concatenated together with a semicolon.
    -}
    extractTextFromLanguageMap lang langMap
        |> String.join "; "


extractTextFromLanguageMap : Language -> LanguageMap -> List String
extractTextFromLanguageMap lang langMap =
    {-
       Returns a list of strings for the best matching language in the map.

       Preference order:
         1) Match the user's language (LanguageCode).
         2) Match values marked as None.
         3) Match values marked as Default.
         4) Fall back to the first value in the map.
    -}
    oneOfMaybes
        [ \_ ->
            case lang of
                LanguageCode code ->
                    find
                        (\languageValues ->
                            case languageValues of
                                LanguageValues (LanguageCode l) _ ->
                                    l == code

                                _ ->
                                    False
                        )
                        langMap

                _ ->
                    Nothing
        , \_ -> find (\(LanguageValues l _) -> l == None) langMap
        , \_ -> find (\(LanguageValues l _) -> l == Default) langMap
        , \_ -> List.head langMap
        ]
        langMap
        |> Maybe.map (\(LanguageValues _ v) -> v)
        |> Maybe.withDefault [ "[No language value found]" ]
