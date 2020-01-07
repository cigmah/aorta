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
    , onSuccessMessage : a -> Html msg
    , children : List (Html msg)
    , submitButtonText : String
    , responseWebData : WebData a -- only used for loading and error styling
    }


errors : Data a msg -> WebData a -> Html msg
errors data webData =
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
                                    "We didn't find any results which matched your criteria. Sorry! We're adding content as fast as we can."

                                _ ->
                                    "There was a bad status with code " ++ String.fromInt int ++ "."

                        BadBody string ->
                            "We had a decoding error. Let us know, this shouldn't happen."
            in
            div [ class "form-errors" ] [ text errorString ]

        Success a ->
            div
                [ class "form-success" ]
                [ data.onSuccessMessage a ]

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
        , errors data data.responseWebData
        ]
