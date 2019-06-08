module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Question exposing (Question)
import Types exposing (..)


view : Model -> Html Msg
view model =
    case model.screen of
        StartScreen ->
            div [] [ text "Welcome", button [ onClick UserClickedStart ] [ text "Start" ] ]

        QuestionScreen questionPhase question ->
            viewQuestionScreen model questionPhase question


viewQuestionScreen : Model -> QuestionPhase -> Question -> Html Msg
viewQuestionScreen model questionPhase question =
    div [] [ text "Question" ]
