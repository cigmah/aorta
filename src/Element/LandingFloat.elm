module Element.LandingFloat exposing (..)

{-| A floating box on the landing page, or a full-width box on mobile.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { tagline : String
    , contents : List (Html msg)
    }


element : Data msg -> Html msg
element data =
    article
        [ class "landing-float" ]
        [ h1
            [ class "landing-float-tagline" ]
            [ text data.tagline ]
        , section
            [ class "landing-float-body" ]
            data.contents
        ]
