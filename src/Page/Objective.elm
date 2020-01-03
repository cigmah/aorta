module Page.Objective exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Element.ObjectiveDetail as ObjectiveDetail
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Utils as Utils exposing (withCmd, withCmdNone)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Objective as Objective
import Types.Paginated as Paginated exposing (Paginated)
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)



-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------


{-| The page `Model` type.
-}
type alias Model =
    { session : Session
    , objectiveWebData : WebData Objective.GetData
    , objectiveId : Int
    , editableData : Maybe Objective.EditableData
    , editing : Bool
    , questionWebData : WebData (Paginated Question.GetBasicData)
    , questionPage : Int
    , errors : Errors
    }


{-| Errors for this page.
-}
type alias Errors =
    {}



-------------------------------------------------------------------------------
-- MSG
-------------------------------------------------------------------------------


{-| The page `Msg` type.
-}
type Msg
    = NoOp
    | GotObjectiveResponse (WebData Objective.GetData)
    | GotQuestionListResponse (WebData (Paginated Question.GetBasicData))
    | ClickedEdit
    | ChangedTitle String
    | ChangedNotes String
    | ClickedSubmitEdits
    | ClickedAddQuestion
    | ChangedStem
    | ClickedAddDistractor
    | ChangedOptionContent Int String
    | ChangedOptionExplanation Int String
    | ClickedSubmitQuestion
    | GotQuestionAddResponse (WebData ())



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


{-| The page initialisation function.
-}
init : Session -> Int -> ( Model, Cmd Msg )
init session objectiveId =
    let
        model =
            { session = session
            , errors = defaultErrors
            , objectiveWebData = Loading
            , objectiveId = objectiveId
            , questionPage = 1
            , questionWebData = Loading
            , editableData = Nothing
            , editing = False
            }
    in
    ( model
    , Cmd.batch
        [ requestObjective model
        , requestQuestionList model
        ]
    )



-------------------------------------------------------------------------------
-- EJECT/INJECT
-------------------------------------------------------------------------------


{-| Ejects the session out of the page.

This function is the same for all pages and should not be changed.

-}
eject : Model -> Session
eject model =
    model.session


{-| Injects a new session into the page.

This function is the same for all pages and should not be changed.

-}
inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )



-------------------------------------------------------------------------------
-- SUBSCRIPTIONS
-------------------------------------------------------------------------------


{-| Subscriptions for this page.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-------------------------------------------------------------------------------
-- UPDATE
-------------------------------------------------------------------------------


{-| Updates the page `Model` from a received `Msg`.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotObjectiveResponse webData ->
            -- This is also triggered when the edit PATCH request is sent, so need to be careful
            case model.editableData of
                -- If there was no edit data, then safe to do the whole replacement
                Nothing ->
                    webData
                        |> updateObjectiveWebData model
                        |> withCmdNone

                -- If there was edit data, then replace and remove editing information if success, or keep if not
                Just editData ->
                    case webData of
                        Success objective ->
                            webData
                                |> updateObjectiveWebData model
                                |> resetEdits
                                |> withCmdNone

                        _ ->
                            webData
                                |> updateObjectiveWebData model
                                |> withCmdNone

        GotQuestionListResponse webData ->
            webData
                |> updateQuestionWebData model
                |> withCmdNone

        ClickedEdit ->
            model
                |> startEdits
                |> withCmdNone

        ChangedTitle string ->
            string
                |> updateEditableTitle model
                |> withCmdNone

        ChangedNotes string ->
            string
                |> updateEditableNotes model
                |> withCmdNone

        ClickedSubmitEdits ->
            Loading
                |> updateObjectiveWebData model
                |> withCmd (requestEditObjective model)

        ClickedAddQuestion ->
            -- TODO
            ( model, Cmd.none )

        ChangedStem ->
            -- TODO
            ( model, Cmd.none )

        ClickedAddDistractor ->
            -- TODO
            ( model, Cmd.none )

        ChangedOptionContent int string ->
            -- TODO
            ( model, Cmd.none )

        ChangedOptionExplanation int string ->
            -- TODO
            ( model, Cmd.none )

        ClickedSubmitQuestion ->
            -- TODO
            ( model, Cmd.none )

        GotQuestionAddResponse response ->
            -- TODO
            ( model, Cmd.none )



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        title =
            case model.objectiveWebData of
                Success objective ->
                    objective.title

                _ ->
                    "Loading Objective..."
    in
    { title = title
    , body = viewBody model
    }


{-| Render the page body from an immutable model view.
-}
viewBody : Model -> List (Html Msg)
viewBody model =
    [ ObjectiveDetail.element
        { objective = model.objectiveWebData
        , editable = isContributor model.objectiveWebData model
        , onClickEdit = ClickedEdit
        , editableData = model.editableData
        , editing = model.editing
        , onChangeTitle = ChangedTitle
        , onChangeNotes = ChangedNotes
        , onClickSubmit = ClickedSubmitEdits
        , canAddQuestion = Session.isUser model.session
        , onClickAddQuestion = ClickedAddQuestion
        }
    ]



-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------


{-| Default errors for this page.
-}
defaultErrors : Errors
defaultErrors =
    {}


{-| Request the objective by ID.
-}
requestObjective : Model -> Cmd Msg
requestObjective model =
    Request.getObjective
        { id = model.objectiveId
        , auth = model.session.auth
        , callback = GotObjectiveResponse
        }


{-| Request the paginated question list for the objective.
-}
requestQuestionList : Model -> Cmd Msg
requestQuestionList model =
    Request.getQuestionBasicList
        { objectiveId = model.objectiveId
        , page = model.questionPage
        , auth = model.session.auth
        , callback = GotQuestionListResponse
        }


{-| Request updates for the edited objective.
-}
requestEditObjective : Model -> Cmd Msg
requestEditObjective model =
    case model.editableData of
        Just editable ->
            Request.patchObjective
                { data = editable
                , objectiveId = model.objectiveId
                , auth = model.session.auth
                , callback = GotObjectiveResponse
                }

        Nothing ->
            Cmd.none


{-| Update the objective response web data.
-}
updateObjectiveWebData : Model -> WebData Objective.GetData -> Model
updateObjectiveWebData model data =
    { model | objectiveWebData = data }


{-| Update the question response web data.
-}
updateQuestionWebData : Model -> WebData (Paginated Question.GetBasicData) -> Model
updateQuestionWebData model data =
    { model | questionWebData = data }


{-| Resets edits on the objective
-}
resetEdits : Model -> Model
resetEdits model =
    { model | editing = False, editableData = Nothing }


{-| Initialise editing.
-}
startEdits : Model -> Model
startEdits model =
    case model.objectiveWebData of
        Success objective ->
            { model | editing = True, editableData = Just (Objective.editableFromData objective) }

        _ ->
            model


{-| Changes editable data title.
-}
updateEditableTitle : Model -> String -> Model
updateEditableTitle model string =
    case model.editableData of
        Just editable ->
            { model | editableData = Just { editable | title = string } }

        _ ->
            model


updateEditableNotes : Model -> String -> Model
updateEditableNotes model string =
    case model.editableData of
        Just editable ->
            { model | editableData = Just { editable | notes = string } }

        _ ->
            model


{-| Checks if the user is the original contributor.
-}
isContributor : WebData Objective.GetData -> Model -> Bool
isContributor webData model =
    case ( webData, model.session.auth ) of
        ( Success objective, User user ) ->
            if objective.contributor.username == user.username then
                True

            else
                False

        _ ->
            False
