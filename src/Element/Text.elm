module Element.Text exposing (..)

{-| Different types of generic texts.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


smallHeader : String -> Html msg
smallHeader string =
    h3
        [ class "small-header" ]
        [ text string ]


{-| Regular body text, wrapped in a <p>
-}
body : String -> Html msg
body string =
    p
        [ class "body-text" ]
        [ text string ]


{-| Data to show credentials on registration.
-}
type alias CredentialShowData =
    { username : String
    , password : String
    }


credentialShow : CredentialShowData -> Html msg
credentialShow data =
    table
        [ class "credential-show" ]
        [ tr
            []
            [ th [] [ text "Username" ]
            , td [] [ text data.username ]
            ]
        , tr []
            [ th [] [ text "Access Code" ]
            , td [] [ text data.password ]
            ]
        ]
