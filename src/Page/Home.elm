module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| AORTA's home page, under `/` or `/index.html`.

This is the homepage that users will see when they visit the root URL. As a
principle, there should be minimal ceremony or friction to starting a
multiple choice test. Login should not be necessary, nor focal on the
homepage.

-}

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element.BackgroundImage as BackgroundImage
import Element.CheckwordList as CheckwordList exposing (CheckwordData, Direction(..), deselectAll, selectAll, updateCheckword)
import Element.Form as Form
import Element.LandingFloat as LandingFloat
import Element.PrimaryButton as PrimaryButton
import Element.TwoColumn as TwoColumn
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import List.Extra
import Page.Utils as Utils exposing (withCmdNone)
import RemoteData exposing (RemoteData(..), WebData)
import TypedSvg
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Icon as Icon
import Types.Interface exposing (Enumerable)
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Stage as Stage exposing (Stage)
import Types.System as System exposing (System)
import Types.Test as Test
import Types.Topic as Topic exposing (Topic)



-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------


{-| The page `Model` type.
-}
type alias Model =
    { session : Session
    , errors : Errors
    , systemDict : Dict Int (CheckwordData Msg)
    , topicDict : Dict Int (CheckwordData Msg)
    , stageDict : Dict Int (CheckwordData Msg)
    , questionIdListResponse : WebData (List Int)
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
    | ClickedSystem Int Bool
    | ClickedTopic Int Bool
    | ClickedStage Int Bool
    | ClickedSelectAll Filter
    | ClickedDeselectAll Filter
    | ClickedStartQuestions
    | GotQuestionIdList (WebData (List Int))



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


{-| The page initialisation function.
-}
init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , errors = defaultErrors
      , systemDict = defaultSystemDict
      , topicDict = defaultTopicDict
      , stageDict = defaultStageDict
      , questionIdListResponse = NotAsked
      }
    , Cmd.none
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
update msg ({ errors } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedSystem system bool ->
            model.systemDict
                |> Dict.update system (Maybe.map (updateCheckword bool))
                |> updateSystemDict model
                |> withCmdNone

        ClickedTopic topic bool ->
            model.topicDict
                |> Dict.update topic (Maybe.map (updateCheckword bool))
                |> updateTopicDict model
                |> withCmdNone

        ClickedStage stage bool ->
            model.stageDict
                |> Dict.update stage (Maybe.map (updateCheckword bool))
                |> updateStageDict model
                |> withCmdNone

        ClickedSelectAll filter ->
            case filter of
                FilterSystem ->
                    model.systemDict
                        |> selectAll
                        |> updateSystemDict model
                        |> withCmdNone

                FilterTopic ->
                    model.topicDict
                        |> selectAll
                        |> updateTopicDict model
                        |> withCmdNone

                FilterStage ->
                    model.stageDict
                        |> selectAll
                        |> updateStageDict model
                        |> withCmdNone

        ClickedDeselectAll filter ->
            case filter of
                FilterSystem ->
                    model.systemDict
                        |> deselectAll
                        |> updateSystemDict model
                        |> withCmdNone

                FilterTopic ->
                    model.topicDict
                        |> deselectAll
                        |> updateTopicDict model
                        |> withCmdNone

                FilterStage ->
                    model.stageDict
                        |> deselectAll
                        |> updateStageDict model
                        |> withCmdNone

        ClickedStartQuestions ->
            let
                newModel =
                    updateQuestionIdListResponse Loading model
            in
            ( newModel, requestQuestionIdList newModel )

        -- The logic for starting a test is a little bit involved, so need to be careful here
        GotQuestionIdList response ->
            case response of
                -- if the question list was received
                Success questions ->
                    -- if there was at least one question, save the list into the test session and reroute
                    if List.length questions > 0 then
                        let
                            ( newSession, newRoute ) =
                                Session.createTest questions Route.Home model.session
                        in
                        ( { model | session = newSession }, Navigation.pushUrl newSession.key (Route.toString newRoute) )
                        -- otherwise, display it as if it were an error, even though the request succeeded

                    else
                        let
                            intercepted =
                                Failure (BadStatus 404)
                        in
                        updateQuestionIdListResponse intercepted model
                            |> withCmdNone

                -- otherwise, just store it in the model for display
                _ ->
                    updateQuestionIdListResponse response model
                        |> withCmdNone



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Home"
    , body = viewBody model
    }


{-| Render the page body from an immutable model view.
-}
viewBody : Model -> List (Html Msg)
viewBody model =
    [ BackgroundImage.element model.session.resources.landingImage
    , LandingFloat.element
        { tagline = "A free and open-source medical question bank."
        , contents =
            [ Form.element
                { onSubmit = ClickedStartQuestions
                , submitButtonText = "Start Questions"
                , responseWebData = model.questionIdListResponse
                , onSuccessMessage = \_ -> div [] []
                , children =
                    [ CheckwordList.element
                        { label = "Year Level(s)"
                        , onSelectAll = ClickedSelectAll FilterStage
                        , onDeselectAll = ClickedDeselectAll FilterStage
                        , dict = model.stageDict
                        , direction = Horizontal
                        }
                    , TwoColumn.element
                        { first =
                            CheckwordList.element
                                { label = "System(s)"
                                , onSelectAll = ClickedSelectAll FilterSystem
                                , onDeselectAll = ClickedDeselectAll FilterSystem
                                , dict = model.systemDict
                                , direction = Vertical
                                }
                        , second =
                            CheckwordList.element
                                { label = "Topic(s)"
                                , onSelectAll = ClickedSelectAll FilterTopic
                                , onDeselectAll = ClickedDeselectAll FilterTopic
                                , dict = model.topicDict
                                , direction = Vertical
                                }
                        }
                    ]
                }
            ]
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


requestQuestionIdList : Model -> Cmd Msg
requestQuestionIdList model =
    Request.getQuestionIdList
        { auth = model.session.auth
        , systemFilters = CheckwordList.filterChecked model.systemDict
        , topicFilters = CheckwordList.filterChecked model.topicDict
        , stageFilters = CheckwordList.filterChecked model.stageDict
        , callback = GotQuestionIdList
        }


{-| A default system dictionary mapping system ints to checkword data.
-}
defaultSystemDict : Dict Int (CheckwordData Msg)
defaultSystemDict =
    CheckwordList.defaultDictFromEnumerable System.enumerable ClickedSystem


{-| A default topic dictionary mapping topic ints to checkword data.
-}
defaultTopicDict : Dict Int (CheckwordData Msg)
defaultTopicDict =
    CheckwordList.defaultDictFromEnumerable Topic.enumerable ClickedTopic


{-| A default training stage dictionary mapping training stage ints to checkword data.
-}
defaultStageDict : Dict Int (CheckwordData Msg)
defaultStageDict =
    CheckwordList.defaultDictFromEnumerable Stage.enumerable ClickedStage
        |> deselectAll


{-| Update the system dictionary with a new one in the model
-}
updateSystemDict : Model -> Dict Int (CheckwordData Msg) -> Model
updateSystemDict model updatedDict =
    { model | systemDict = updatedDict }


{-| Update the topic dictionary with a new one in the model
-}
updateTopicDict : Model -> Dict Int (CheckwordData Msg) -> Model
updateTopicDict model updatedDict =
    { model | topicDict = updatedDict }


{-| Update the stage dictionary with a new one in the model
-}
updateStageDict : Model -> Dict Int (CheckwordData Msg) -> Model
updateStageDict model updatedDict =
    { model | stageDict = updatedDict }


updateQuestionIdListResponse : WebData (List Int) -> Model -> Model
updateQuestionIdListResponse response model =
    { model | questionIdListResponse = response }


{-| The types of categorisation of questions.
-}
type Filter
    = FilterSystem
    | FilterStage
    | FilterTopic
