module Element.PaginatedResults exposing (..)

{-| A view of paginated, retrieved results.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Maybe.Extra exposing (isNothing)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Paginated as Paginated exposing (Paginated)


type alias Data a msg =
    { webData : WebData (Paginated a)
    , page : Int -- the current page
    , onClickNext : msg
    , onClickPrev : msg
    , itemToElement : a -> Html msg
    }


element : Data a msg -> Html msg
element data =
    case data.webData of
        Success results ->
            successView results data

        Loading ->
            loadingView

        NotAsked ->
            notAskedView

        Failure e ->
            failureView e


successView : Paginated a -> Data a msg -> Html msg
successView results data =
    section
        [ class "paginated-results" ]
        [ header
            [ class "paginated-results-header" ]
            [ h1
                [ class "paginated-results-header-count" ]
                [ text <| String.fromInt results.count ++ " results." ]
            , div
                [ class "paginated-results-header-buttons" ]
                [ button
                    [ class "paginated-results-header-button previous"
                    , onClick data.onClickPrev
                    , disabled (isNothing results.previous)
                    ]
                    [ text "Prev" ]
                , div
                    [ class "paginated-results-header-page" ]
                    [ text <| "Page " ++ String.fromInt data.page ]
                , button
                    [ class "paginated-results-header-button next"
                    , onClick data.onClickNext
                    , disabled (isNothing results.next)
                    ]
                    [ text "Next" ]
                ]
            ]
        , ul
            [ class "paginated-results-results" ]
            (List.map (data.itemToElement >> (\x -> li [ class "paginated-results-item" ] [ x ])) results.results)
        ]


loadingView : Html msg
loadingView =
    section
        [ class "paginated-results" ]
        [ section
            [ class "paginated-results-text loading" ]
            [ text "Loading..." ]
        ]


notAskedView : Html msg
notAskedView =
    section
        [ class "paginated-results" ]
        [ section
            [ class "paginated-results-text not-asked" ]
            [ text "Type a search query and click Search." ]
        ]


failureView : Error -> Html msg
failureView error =
    let
        errorString =
            case error of
                BadUrl string ->
                    "The requested URL was invalid. Let us know, this shouldn't happen. URL requested was: " ++ string

                Timeout ->
                    "The request timed out. We might be having some downtime, so let us know or check back later."

                NetworkError ->
                    "We couldn't connect. We might be having some downtime, so let us know or check back later."

                BadStatus int ->
                    case int of
                        404 ->
                            "The requested resource wasn't found. Let us know, this shouldn't happen."

                        _ ->
                            "There was a bad status with code " ++ String.fromInt int ++ "."

                BadBody string ->
                    "We had a decoding error. Let us know, this shouldn't happen."
    in
    section
        [ class "paginated-results" ]
        [ section
            [ class "paginated-results-text error" ]
            [ text errorString ]
        ]
