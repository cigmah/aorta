module Element.FilterAndResults exposing (..)

{-| A containerised responsive layout with filters on the side and results on the right.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Data msg =
    { side : List (Html msg)
    , main : List (Html msg)
    , filtersVisible : Bool
    , toggleViewFilters : msg -- toggles view side, only activates in mobile
    }


element : Data msg -> Html msg
element data =
    section
        [ class "filter-and-results" ]
        [ section
            [ class "filter-and-results-side" ]
            [ button
                [ class "filter-and-results-side-header"
                , onClick data.toggleViewFilters
                ]
                [ text "Filters" ]
            , section
                [ class "filter-and-results-side-body"
                , classList [ ( "visible", data.filtersVisible ) ]
                ]
                data.side
            ]
        , section
            [ class "filter-and-results-main" ]
            data.main
        ]
