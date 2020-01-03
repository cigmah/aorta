module Element.QuestionResult exposing (..)

{-| A result for a single question shown on a paginated list of question results.
-}

import Architecture.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Question as Question


element : Question.GetBasicData -> Html msg
element data =
    a
        [ Route.toHref (Route.Question data.id)
        , class "question-result"
        ]
        [ section
            [ class "question-result-stem" ]
            [ text data.stem ]
        , div
            [ class "question-result-contributor" ]
            [ text ("Contributed by " ++ data.contributor.username) ]
        ]
