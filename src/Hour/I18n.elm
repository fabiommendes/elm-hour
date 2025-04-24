module Hour.I18n exposing (en)

{-| This module declares all supported languages and their formatting information.


## Supported languages

@docs en

-}

import Hour exposing (Language)


{-| en-US support
-}
en : Language
en =
    { am = "am."
    , pm = "pm."
    }
