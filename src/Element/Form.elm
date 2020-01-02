module Element.Form exposing (..)

{-| A form with a submit button.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..), WebData)


type alias Data a msg =
    { onSubmit : msg
    , children : List (Html msg)
    , submitButtonText : String
    , responseWebData : WebData a -- only used for loading and error styling
    }


errors : WebData a -> Html msg
errors webData =
    case webData of
        Failure e ->
            let
                errorString =
                    case e of
                        BadUrl string ->
                            "The requested URL was invalid. Let us know, this shouldn't happen. URL requested was: " ++ string

                        Timeout ->
                            "The request timed out. We might be having some downtime, so let us know or check back later."

                        NetworkError ->
                            "We couldn't connect. We might be having some downtime, so let us know or check back later."

                        BadStatus int ->
                            case int of
                                409 ->
                                    "We already have some of the information you entered, so there was a clash. Try changing your inputs."

                                404 ->
                                    "The requested resource wasn't found. Let us know, this shouldn't happen."

                                _ ->
                                    "There was a bad status with code " ++ String.fromInt int ++ "."

                        BadBody string ->
                            "We had a decoding error. Let us know, this shouldn't happen."
            in
            div [ class "form-errors" ] [ text errorString ]

        _ ->
            div [] []


element : Data a msg -> Html msg
element data =
    let
        submitButtonText =
            case data.responseWebData of
                Loading ->
                    "Please wait..."

                _ ->
                    data.submitButtonText

        isLoading =
            case data.responseWebData of
                Loading ->
                    True

                _ ->
                    False
    in
    Html.form
        [ class "form"
        , onSubmit data.onSubmit
        ]
        [ section
            [ class "form-body" ]
            data.children
        , button
            [ class "form-submit-button"
            , type_ "submit"
            , disabled isLoading
            , classList [ ( "loading", isLoading ) ]
            ]
            [ text submitButtonText ]
        , errors data.responseWebData
        ]
