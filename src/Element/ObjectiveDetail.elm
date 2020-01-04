module Element.ObjectiveDetail exposing (..)

{-| A detailed view of an objectve.
-}

import Architecture.Route as Route
import Element.AddQuestionModal as AddQuestionModal
import Element.Empty as Empty
import Element.GhostButton as GhostButton
import Element.PaginatedResults as PaginatedResults
import Element.QuestionResult as QuestionResult
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Markdown
import Maybe.Extra exposing (isNothing)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Icon as Icon
import Types.Objective as Objective
import Types.Paginated as Paginated exposing (Paginated)
import Types.Question as Question
import Types.Specialty as Specialty
import Types.Stage as Stage
import Types.Topic as Topic


type alias Data msg =
    { objective : WebData Objective.GetData
    , editable : Bool
    , onClickEdit : msg
    , editableData : Maybe Objective.EditableData
    , editing : Bool
    , onChangeTitle : String -> msg
    , onChangeNotes : String -> msg
    , onClickSubmit : msg
    , canAddQuestion : Bool
    , onClickAddQuestion : msg
    , showAddQuestionModal : Bool
    , questionWebData : WebData (Paginated Question.GetBasicData)
    , questionPage : Int
    , paginatedOnClickNext : msg
    , paginatedOnClickPrev : msg
    , onClickQuestion : Int -> msg
    , addQuestion :
        { question : Question.PostData
        , response : WebData Question.GetBasicData
        , onClickClose : msg
        , onChangeStem : String -> msg
        , onChangeChoiceContent : Int -> String -> msg
        , onChangeChoiceExplanation : Int -> String -> msg
        , onClickAddChoice : msg
        , onClickRemoveChoice : Int -> msg
        , onClickSubmit : msg
        }
    }


element : Data msg -> Html msg
element data =
    case data.objective of
        Loading ->
            if data.editing then
                case data.editableData of
                    Just editable ->
                        editView editable data

                    _ ->
                        loadingView

            else
                loadingView

        NotAsked ->
            notAskedView

        Failure e ->
            -- Was editing and the PUT request failed
            if data.editing then
                case data.editableData of
                    Just editable ->
                        editView editable data

                    _ ->
                        failureView e

            else
                failureView e

        Success objective ->
            if data.editing then
                case data.editableData of
                    Just editable ->
                        editView editable data

                    _ ->
                        successView objective data

            else
                successView objective data


loadingView : Html msg
loadingView =
    section
        [ class "objective-text loading" ]
        [ text "Loading..." ]


notAskedView : Html msg
notAskedView =
    section
        [ class "objective-text not-asked" ]
        [ text "Hm. The request wasn't sent. This shouldn't happen, please let us know." ]


failureView : Error -> Html msg
failureView e =
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
                        404 ->
                            "The requested resource wasn't found. Let us know, this shouldn't happen."

                        _ ->
                            "There was a bad status with code " ++ String.fromInt int ++ "."

                BadBody string ->
                    "We had a decoding error. Let us know, this shouldn't happen."
    in
    section
        [ class "objective-text not-asked" ]
        [ text errorString ]


successView : Objective.GetData -> Data msg -> Html msg
successView objective data =
    let
        editButton =
            if data.editable then
                if data.editing then
                    Empty.element

                else
                    button
                        [ class "objective-sidebar-edit-button"
                        , onClick data.onClickEdit
                        ]
                        [ text "Edit" ]

            else
                Empty.element

        notes =
            if objective.notes == "" then
                [ text "There are no notes for this objective yet." ]

            else
                Markdown.toHtml Nothing objective.notes

        addQuestionButton =
            if data.canAddQuestion then
                button
                    [ class "objective-sidebar-add-question-button"
                    , onClick data.onClickAddQuestion
                    ]
                    [ text "Add Question" ]

            else
                Empty.element

        addQuestionModal =
            if data.showAddQuestionModal then
                AddQuestionModal.element
                    { question = data.addQuestion.question
                    , questionWebData = data.addQuestion.response
                    , objectiveTitle = objective.title
                    , onClickClose = data.addQuestion.onClickClose
                    , onChangeStem = data.addQuestion.onChangeStem
                    , onChangeChoiceContent = data.addQuestion.onChangeChoiceContent
                    , onChangeChoiceExplanation = data.addQuestion.onChangeChoiceExplanation
                    , onClickAddChoice = data.addQuestion.onClickAddChoice
                    , onClickRemoveChoice = data.addQuestion.onClickRemoveChoice
                    , onClickSubmit = data.addQuestion.onClickSubmit
                    }

            else
                Empty.element
    in
    section
        [ class "objective" ]
        [ section
            [ class "objective-sidebar" ]
            [ div
                [ class "objective-sidebar-icon" ]
                [ Specialty.toIcon objective.specialty ]
            , a
                [ class "objective-sidebar-back-button"
                , Route.toHref Route.ObjectiveList
                ]
                [ text "Back" ]
            , editButton
            , addQuestionButton
            ]
        , section
            [ class "objective-main" ]
            [ article
                [ class "objective-body" ]
                [ header
                    [ class "objective-body-header" ]
                    [ h1
                        [ class "objective-body-title" ]
                        [ text objective.title ]
                    , div
                        [ class "objective-body-tags" ]
                        [ div
                            [ class "objective-tag stage" ]
                            [ text (Stage.enumerable.toString objective.stage) ]
                        , div
                            [ class "objective-tag specialty" ]
                            [ text (Specialty.enumerable.toString objective.specialty) ]
                        , div
                            [ class "objective-tag topic" ]
                            [ text (Topic.enumerable.toString objective.topic) ]
                        ]
                    , div
                        [ class "objective-body-contributor" ]
                        [ text "Contributed by "
                        , span
                            [ class "objective-body-contributor-username" ]
                            [ text objective.contributor.username ]
                        ]
                    ]
                , section
                    [ class "objective-body-notes markdown" ]
                    notes
                ]
            , section
                [ class "objective-questions" ]
                [ h1
                    [ class "objective-questions-heading" ]
                    [ text "Attached Questions" ]
                , PaginatedResults.element
                    { webData = data.questionWebData
                    , page = data.questionPage
                    , onClickNext = data.paginatedOnClickNext
                    , onClickPrev = data.paginatedOnClickPrev
                    , itemToElement = QuestionResult.element data.onClickQuestion
                    }
                ]
            ]
        , addQuestionModal
        ]


isLoading : WebData a -> Bool
isLoading webData =
    case webData of
        Loading ->
            True

        _ ->
            False


editView : Objective.EditableData -> Data msg -> Html msg
editView editable data =
    let
        errorDiv =
            case data.objective of
                Failure e ->
                    div
                        [ class "objective-body-edit-error" ]
                        [ text (editErrorString e) ]

                _ ->
                    Empty.element
    in
    Html.form
        [ class "objective-edit-form"
        , onSubmit data.onClickSubmit
        ]
        [ header
            [ class "objective-edit-header" ]
            [ textarea
                [ class "objective-body-title"
                , required True
                , onInput data.onChangeTitle
                , value editable.title
                , placeholder "Learning objective..."
                , rows 2
                ]
                []
            ]
        , textarea
            [ class "objective-body-notes markdown"
            , required False
            , onInput data.onChangeNotes
            , value editable.notes
            , placeholder "Notes..."
            , rows 20
            ]
            []
        , button
            [ type_ "submit"
            , disabled (isLoading data.objective)
            , class "objective-body-edit-submit"
            ]
            [ text "Save" ]
        , errorDiv
        ]


editErrorString : Error -> String
editErrorString error =
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
