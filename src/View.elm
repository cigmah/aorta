module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Markdown.Config exposing (HtmlOption(..))
import Question exposing (..)
import Types exposing (..)


view : Model -> Html Msg
view model =
    let
        body =
            case model.screen of
                StartScreen ->
                    viewStartScreen

                LoadingScreen ->
                    viewLoadingScreen

                QuestionScreen questionPhase question ->
                    viewQuestionScreen model questionPhase question
    in
    div [ class "container" ] [ body ]



-- By Screen


viewStartScreen : Html Msg
viewStartScreen =
    div [ class "start-menu" ]
        [ h1 [ class "title" ] [ text "MERT" ]
        , h2 [ class "subtitle" ] [ text "A free and open source Mini EMQ Revision Tool." ]
        , button [ onClick UserClickedStart ] [ text "Start" ]
        ]


viewLoadingScreen : Html Msg
viewLoadingScreen =
    div [] []


viewQuestionScreen : Model -> QuestionPhase -> QuestionView -> Html Msg
viewQuestionScreen model questionPhase question =
    div [ class "question-screen" ]
        [ viewQuestion model questionPhase question ]



-- Markdown Helpers


markdownOptions : Maybe Markdown.Config.Options
markdownOptions =
    Just { softAsHardLineBreak = False, rawHtml = DontParse }


markdown : String -> List (Html Msg)
markdown raw =
    Markdown.toHtml markdownOptions raw



-- Components


viewQuestion : Model -> QuestionPhase -> QuestionView -> Html Msg
viewQuestion model questionPhase question =
    div [ class "question" ]
        [ div [ class "stem" ] (markdown question.stem)
        , div [ class "choice-list" ]
            (List.indexedMap (viewChoice questionPhase)
                question.choices
            )
        ]


viewChoice : QuestionPhase -> Int -> ChoiceView -> Html Msg
viewChoice questionPhase index choice =
    button [ class "choice", onClick (UserClickedResponse choice) ]
        [ span [ class "index" ] [ text <| String.fromInt index ]
        , span [ class "description" ] (markdown <| choiceDescription choice)
        ]
