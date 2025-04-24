module Hour.I18n exposing
    ( Language
    , en
    )

{-| This module declares all supported languages and their formatting information.

@docs Language


## Supported languages

@docs en

-}


{-| Opaque data structure that stores formatting information about a language.
-}
type alias Language =
    { am : String
    , pm : String
    }


{-| en-US support
-}
en : Language
en =
    { am = "am."
    , pm = "pm."
    }
