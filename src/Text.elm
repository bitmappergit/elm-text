module Text exposing (..)

import Regex exposing (Regex)


type Text
    = Invalid String
    | Valid String
    | Empty


{-| Validates a `String` given a validation function, and returns a `Text`
-}
validate : (String -> Bool) -> String -> Text
validate validator string =
    if string == "" then
        Empty

    else if validator string then
        Valid string

    else
        Invalid string


{-| Extracts a `String` from a `Text`
-}
toString : Text -> String
toString text =
    case text of
        Empty ->
            ""

        Valid string ->
            string

        Invalid string ->
            string


{-| -}
isValid : Text -> Bool
isValid text =
    case text of
        Valid _ ->
            True

        _ ->
            False


{-| -}
isInvalid : Text -> Bool
isInvalid text =
    case text of
        Invalid _ ->
            True

        _ ->
            False


{-| -}
isEmpty : Text -> Bool
isEmpty text =
    case text of
        Empty ->
            True

        _ ->
            False


{-| -}
maybeValid : Text -> a -> Maybe a
maybeValid text result =
    if isValid text then
        Just result

    else
        Nothing


{-| -}
allMaybeValid : List Text -> a -> Maybe a
allMaybeValid text result =
    if List.all isValid text then
        Just result

    else
        Nothing


{-| -}
alwaysValid : String -> Text
alwaysValid =
    validate <| always True


{-| Email validation `Regex`
-}
emailRegex : Regex
emailRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "^(?:(?:[\\w`~!#$%^&*\\-=+;:{}'|,?\\/]+(?:(?:\\.(?:\"(?:\\\\?[\\w`~!#$%^&*\\-=+;:{}'|,?\\/\\.()<>\\[\\] @]|\\\\\"|\\\\\\\\)*\"|[\\w`~!#$%^&*\\-=+;:{}'|,?\\/]+))*\\.[\\w`~!#$%^&*\\-=+;:{}'|,?\\/]+)?)|(?:\"(?:\\\\?[\\w`~!#$%^&*\\-=+;:{}'|,?\\/\\.()<>\\[\\] @]|\\\\\"|\\\\\\\\)+\"))@(?:[a-zA-Z\\d\\-]+(?:\\.[a-zA-Z\\d\\-]+)*|\\[\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\])$"


{-| Phone validation `Regex`
-}
phoneRegex : Regex
phoneRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "(\\+?( |-|\\.)?\\d{1,2}( |-|\\.)?)?(\\(?\\d{3}\\)?|\\d{3})?( |-|\\.)?(\\d{3}( |-|\\.)?\\d{4})"


isValidEmail : String -> Bool
isValidEmail =
    Regex.contains emailRegex


isValidPhone : String -> Bool
isValidPhone =
    Regex.contains phoneRegex
