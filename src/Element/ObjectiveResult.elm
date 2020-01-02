module Element.ObjectiveResult exposing (..)

{-| An objective shown in search results.
-}

import Architecture.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Icon as Icon
import Types.Objective as Objective
import Types.Specialty as Specialty
import Types.Stage as Stage
import Types.Topic as Topic


element : Objective.GetData -> Html msg
element data =
    a
        [ class "objective-result"
        , Route.toHref (Route.Objective data.id)
        ]
        [ figure
            [ class "objective-result-icon" ]
            [ Specialty.toIcon data.specialty ]
        , section
            [ class "objective-result-body" ]
            [ header
                [ class "objective-result-tags" ]
                [ div
                    [ class "objective-result-tag stage" ]
                    [ text <| Stage.enumerable.toBriefString data.stage ]
                , div
                    [ class "objective-result-tag specialty" ]
                    [ text <| Specialty.enumerable.toBriefString data.specialty ]
                , div
                    [ class "objective-result-tag topic" ]
                    [ text <| Topic.enumerable.toBriefString data.topic ]
                ]
            , div
                [ class "objective-result-title" ]
                [ text data.title ]
            ]
        ]
