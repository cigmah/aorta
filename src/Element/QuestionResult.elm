module Element.QuestionResult exposing (..)

{-| A result for a single question shown on a paginated list of question results.
-}

import Architecture.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Markdown exposing (markdown)
import Types.Question as Question


element : (Int -> msg) -> Question.GetBasicData -> Html msg
element onClickQuestion data =
    button
        [ class "question-result"
        , type_ "button"
        , onClick (onClickQuestion data.id)
        ]
        [ section
            [ class "question-result-stem" ]
            [ markdown data.stem ]
        , div
            [ class "question-result-contributor" ]
            [ text ("Contributed by " ++ data.contributor.username) ]
        ]
