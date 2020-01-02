module Element.SneakyBox exposing (..)

{-| A small nondescript box, for nonobstrusive requests for contribution.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { header : String
    , content : List (Html msg)
    }


element : Data msg -> Html msg
element data =
    details
        [ class "sneaky-box" ]
        [ summary
            [ class "sneaky-box-summary" ]
            [ text data.header ]
        , section
            [ class "sneaky-box-content" ]
            data.content
        ]
