module Page.ObjectiveList exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| An learning objective listing page, under `/objectives`.

This page is for viewing and searching a list of learning objectives.

-}

import Architecture.Route as Route exposing (ObjectiveListQueries)
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element.CheckwordList as CheckwordList exposing (CheckwordData, Direction(..), deselectAll, selectAll, updateCheckword)
import Element.Empty as Empty
import Element.FilterAndResults as FilterAndResults
import Element.Form as Form
import Element.ObjectiveResult as ObjectiveResult
import Element.PaginatedResults as PaginatedResults
import Element.PrimaryButton as PrimaryButton
import Element.SearchBar as SearchBar
import Element.Select as Select
import Element.SneakyBox as SneakyBox
import Element.Text as Text
import Element.TextInput as TextInput
import Element.ThreeColumn as ThreeColumn
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Page.Utils as Utils exposing (withCmd, withCmdNone)
import RemoteData exposing (RemoteData(..), WebData)
import TypedSvg
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Icon as Icon
import Types.Interface exposing (Enumerable)
import Types.Objective as Objective
import Types.Paginated as Paginated exposing (Paginated)
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Stage as Stage exposing (Stage)
import Types.System as System exposing (System)
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
    , results : WebData (Paginated Objective.GetData)
    , filtersVisible : Bool
    , search : String
    , page : Int
    , addObjective : Objective.PostData
    , addObjectiveResponse : WebData Objective.GetData
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
    | ClickedSystem Int Bool -- toggled system filter
    | ClickedTopic Int Bool -- toggled topic filter
    | ClickedStage Int Bool -- toggled stage filter
    | ClickedSelectAll Filter -- clicked select all on a filter
    | ClickedDeselectAll Filter -- clicked deselect all on a filter
    | ClickedToggleFilters -- toggled filter visibility (button only appears on mobile)
    | ChangedSearch String -- changed the string in the seach box
    | ClickedSearch -- submit search to get results
    | GotSearchResults (WebData (Paginated Objective.GetData)) -- receive the search results
    | ClickedNext -- clicked next on paginated results
    | ClickedPrev -- clicked prev on paginated results
    | ChangedAddObjectiveTitle String -- changed the new objective title (only authenticated)
    | ChangedAddObjectiveStage String -- change the new objective stage (only authenticated)
    | ChangedAddObjectiveSystem String -- change the new objective system (only authenticated)
    | ChangedAddObjectiveTopic String -- change the new objective topic (only authenticated)
    | ClickedAddObjective -- submit a new objective
    | GotAddObjectiveResponse (WebData Objective.GetData) -- receive the create-objective response



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


{-| The page initialisation function.
-}
init : Session -> ObjectiveListQueries -> ( Model, Cmd Msg )
init session queries =
    let
        model =
            { session = { session | objectiveListQueries = queries }
            , errors = defaultErrors
            , systemDict = fromCheckedSystems queries.systems
            , topicDict = fromCheckedTopics queries.topics
            , stageDict = fromCheckedStages queries.stages
            , results = Loading
            , filtersVisible = False
            , search = queries.search |> Maybe.withDefault ""
            , page = queries.page |> Maybe.withDefault 1
            , addObjective = Objective.init
            , addObjectiveResponse = NotAsked
            }
    in
    ( model, searchRequest model )



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

        ClickedSystem system bool ->
            model.systemDict
                |> Dict.update system (Maybe.map (updateCheckword bool))
                |> updateSystemDict model

        ClickedTopic topic bool ->
            model.topicDict
                |> Dict.update topic (Maybe.map (updateCheckword bool))
                |> updateTopicDict model

        ClickedStage stage bool ->
            model.stageDict
                |> Dict.update stage (Maybe.map (updateCheckword bool))
                |> updateStageDict model

        ClickedSelectAll filter ->
            case filter of
                FilterSystem ->
                    model.systemDict
                        |> selectAll
                        |> updateSystemDict model

                FilterTopic ->
                    model.topicDict
                        |> selectAll
                        |> updateTopicDict model

                FilterStage ->
                    model.stageDict
                        |> selectAll
                        |> updateStageDict model

        ClickedDeselectAll filter ->
            case filter of
                FilterSystem ->
                    model.systemDict
                        |> deselectAll
                        |> updateSystemDict model

                FilterTopic ->
                    model.topicDict
                        |> deselectAll
                        |> updateTopicDict model

                FilterStage ->
                    model.stageDict
                        |> deselectAll
                        |> updateStageDict model

        ClickedToggleFilters ->
            model.filtersVisible
                |> not
                |> updateFiltersVisible model
                |> withCmdNone

        ChangedSearch string ->
            string
                |> updateSearch model
                |> withCmdNone

        ChangedAddObjectiveTitle string ->
            string
                |> updateAddObjectiveTitle model
                |> withCmdNone

        ChangedAddObjectiveStage stage ->
            stage
                |> String.toInt
                |> Maybe.withDefault 0
                |> Stage.enumerable.fromInt
                |> updateAddObjectiveStage model
                |> withCmdNone

        ChangedAddObjectiveTopic topic ->
            topic
                |> String.toInt
                |> Maybe.withDefault 0
                |> Topic.enumerable.fromInt
                |> updateAddObjectiveTopic model
                |> withCmdNone

        ChangedAddObjectiveSystem system ->
            system
                |> String.toInt
                |> Maybe.withDefault 0
                |> System.enumerable.fromInt
                |> updateAddObjectiveSystem model
                |> withCmdNone

        ClickedAddObjective ->
            Loading
                |> updateAddObjectiveResponse model
                |> withCmd
                    (Request.postObjective
                        { data = model.addObjective
                        , auth = model.session.auth
                        , callback = GotAddObjectiveResponse
                        }
                    )

        GotAddObjectiveResponse response ->
            response
                |> updateAddObjectiveResponse model
                |> withCmdNone

        ClickedSearch ->
            Loading
                |> updateResults model
                |> clickedSearch

        GotSearchResults response ->
            response
                |> updateResults model
                |> withCmdNone

        ClickedNext ->
            updatePage model Increment

        ClickedPrev ->
            updatePage model Decrement



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = "Objectives"
    , body = viewBody model
    }


{-| Render the page body from an immutable model view.
-}
viewBody : Model -> List (Html Msg)
viewBody model =
    let
        sneakyAddObjective =
            case model.session.auth of
                Guest ->
                    Empty.element

                User _ ->
                    viewSneakyBox model
    in
    [ FilterAndResults.element
        { filtersVisible = model.filtersVisible
        , toggleViewFilters = ClickedToggleFilters
        , side =
            [ CheckwordList.element
                { label = "Year Level(s)"
                , onSelectAll = ClickedSelectAll FilterStage
                , onDeselectAll = ClickedDeselectAll FilterStage
                , dict = model.stageDict
                , direction = Vertical
                }
            , CheckwordList.element
                { label = "System(s)"
                , onSelectAll = ClickedSelectAll FilterSystem
                , onDeselectAll = ClickedDeselectAll FilterSystem
                , dict = model.systemDict
                , direction = Vertical
                }
            , CheckwordList.element
                { label = "Topic(s)"
                , onSelectAll = ClickedSelectAll FilterTopic
                , onDeselectAll = ClickedDeselectAll FilterTopic
                , dict = model.topicDict
                , direction = Vertical
                }
            ]
        , main =
            [ SearchBar.element
                { value = model.search
                , placeholder = "Search objectives..."
                , onInput = ChangedSearch
                , onSearch = ClickedSearch
                }
            , sneakyAddObjective
            , PaginatedResults.element
                { webData = model.results
                , page = model.page
                , onClickNext = ClickedNext
                , onClickPrev = ClickedPrev
                , itemToElement = ObjectiveResult.element
                }
            ]
        }
    ]


viewSneakyBox : Model -> Html Msg
viewSneakyBox model =
    SneakyBox.element
        { header = "Interested in helping out?"
        , content =
            [ Text.body "We need help adding and managing learning objectives. If you see one that's missing and are able to contribute notes for it, write a new objective below and you can start adding to it straight away."
            , Form.element
                { onSubmit = ClickedAddObjective
                , submitButtonText = "Add Learning Objective"
                , responseWebData = model.addObjectiveResponse
                , onSuccessMessage = onSuccessAddedObjective
                , children =
                    [ ThreeColumn.element
                        { first =
                            Select.element
                                { label = "Year Level"
                                , value = model.addObjective.stage
                                , enumerable = Stage.enumerable
                                , onInput = ChangedAddObjectiveStage
                                }
                        , second =
                            Select.element
                                { label = "System"
                                , value = model.addObjective.system
                                , enumerable = System.enumerable
                                , onInput = ChangedAddObjectiveSystem
                                }
                        , third =
                            Select.element
                                { label = "Topic"
                                , value = model.addObjective.topic
                                , enumerable = Topic.enumerable
                                , onInput = ChangedAddObjectiveTopic
                                }
                        }
                    , TextInput.element
                        { label = "Learning Objective"
                        , id = "learning-objective-title"
                        , information = "The learning objective you'd like to add."
                        , inputType = "text"
                        , required = True
                        , placeholder = "The learning objective is to...(your objective here)"
                        , value = model.addObjective.title
                        , onInput = ChangedAddObjectiveTitle
                        }
                    ]
                }
            ]
        }



-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------


{-| Default errors for this page.
-}
defaultErrors : Errors
defaultErrors =
    {}


{-| A default system dictionary mapping system ints to checkword data.
-}
defaultSystemDict : Dict Int (CheckwordData Msg)
defaultSystemDict =
    CheckwordList.defaultDictFromEnumerable System.enumerable ClickedSystem


{-| Create system dict from maybe list of checked items.
-}
fromCheckedSystems : Maybe (List Int) -> Dict Int (CheckwordData Msg)
fromCheckedSystems maybeChecked =
    case maybeChecked of
        Nothing ->
            defaultSystemDict

        Just checked ->
            List.foldl
                (\key -> Dict.update key (Maybe.map (updateCheckword True)))
                defaultSystemDict
                checked


{-| Create topics dict from maybe list of checked topics.
-}
fromCheckedTopics : Maybe (List Int) -> Dict Int (CheckwordData Msg)
fromCheckedTopics maybeChecked =
    case maybeChecked of
        Nothing ->
            defaultTopicDict

        Just checked ->
            List.foldl
                (\key -> Dict.update key (Maybe.map (updateCheckword True)))
                defaultTopicDict
                checked


{-| Create stages dict from maybe list of checked topics.
-}
fromCheckedStages : Maybe (List Int) -> Dict Int (CheckwordData Msg)
fromCheckedStages maybeChecked =
    case maybeChecked of
        Nothing ->
            defaultStageDict

        Just checked ->
            List.foldl
                (\key -> Dict.update key (Maybe.map (updateCheckword True)))
                defaultStageDict
                checked


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


{-| Update the system dictionary with a new one in the model
-}
updateSystemDict : Model -> Dict Int (CheckwordData Msg) -> ( Model, Cmd Msg )
updateSystemDict model updatedDict =
    let
        updatedObjectiveListQueries =
            model.session.objectiveListQueries
                |> Route.updateObjectiveListSystems (Just <| CheckwordList.filterChecked updatedDict)
                |> Route.updateObjectiveListPage Nothing
    in
    ( model
    , Navigation.pushUrl model.session.key (Route.toString <| Route.ObjectiveList updatedObjectiveListQueries)
    )


{-| Update the topic dictionary with a new one in the model
-}
updateTopicDict : Model -> Dict Int (CheckwordData Msg) -> ( Model, Cmd Msg )
updateTopicDict model updatedDict =
    let
        updatedObjectiveListQueries =
            model.session.objectiveListQueries
                |> Route.updateObjectiveListTopics (Just <| CheckwordList.filterChecked updatedDict)
                |> Route.updateObjectiveListPage Nothing
    in
    ( model
    , Navigation.pushUrl model.session.key (Route.toString <| Route.ObjectiveList updatedObjectiveListQueries)
    )


{-| Update the stage dictionary with a new one in the model
-}
updateStageDict : Model -> Dict Int (CheckwordData Msg) -> ( Model, Cmd Msg )
updateStageDict model updatedDict =
    let
        updatedObjectiveListQueries =
            model.session.objectiveListQueries
                |> Route.updateObjectiveListStages (Just <| CheckwordList.filterChecked updatedDict)
                |> Route.updateObjectiveListPage Nothing
    in
    ( model
    , Navigation.pushUrl model.session.key (Route.toString <| Route.ObjectiveList updatedObjectiveListQueries)
    )


{-| Update the search bar with a new string
-}
updateSearch : Model -> String -> Model
updateSearch model string =
    { model | search = string }


{-| Update filters visible
-}
updateFiltersVisible : Model -> Bool -> Model
updateFiltersVisible model bool =
    { model | filtersVisible = bool }


{-| Modifies the add objective title string.
-}
updateAddObjectiveTitle : Model -> String -> Model
updateAddObjectiveTitle model string =
    { model | addObjective = model.addObjective |> Objective.updateTitle string }


{-| Modifies the add objective system.
-}
updateAddObjectiveSystem : Model -> System -> Model
updateAddObjectiveSystem model system =
    { model | addObjective = model.addObjective |> Objective.updateSystem system }


{-| Modifies the add objective stage.
-}
updateAddObjectiveStage : Model -> Stage -> Model
updateAddObjectiveStage model stage =
    { model | addObjective = model.addObjective |> Objective.updateStage stage }


{-| Modifies the add objective topic.
-}
updateAddObjectiveTopic : Model -> Topic -> Model
updateAddObjectiveTopic model topic =
    { model | addObjective = model.addObjective |> Objective.updateTopic topic }


{-| Updates the add objective response
-}
updateAddObjectiveResponse : Model -> WebData Objective.GetData -> Model
updateAddObjectiveResponse model response =
    case response of
        Loading ->
            { model | addObjectiveResponse = response }

        Failure _ ->
            { model | addObjectiveResponse = response }

        NotAsked ->
            { model | addObjectiveResponse = response }

        Success _ ->
            { model
                | addObjectiveResponse = response
                , addObjective = model.addObjective |> Objective.updateTitle ""
            }


{-| Clicked the search button.
-}
clickedSearch : Model -> ( Model, Cmd Msg )
clickedSearch model =
    let
        updatedObjectiveListQueries =
            model.session.objectiveListQueries
                |> Route.updateObjectiveListSearch (Just model.search)
                |> Route.updateObjectiveListPage Nothing
    in
    ( model
    , Navigation.pushUrl model.session.key (Route.toString <| Route.ObjectiveList updatedObjectiveListQueries)
    )


{-| Updates the search results
-}
updateResults : Model -> WebData (Paginated Objective.GetData) -> Model
updateResults model response =
    { model | results = response }


{-| Updates the page.
-}
updatePage : Model -> Direction -> ( Model, Cmd Msg )
updatePage model direction =
    let
        newPageNumber =
            case direction of
                Increment ->
                    model.page + 1

                Decrement ->
                    model.page - 1

        updatedObjectiveListQueries =
            model.session.objectiveListQueries
                |> Route.updateObjectiveListPage (Just newPageNumber)
    in
    ( model
    , Navigation.pushUrl model.session.key (Route.toString <| Route.ObjectiveList updatedObjectiveListQueries)
    )


{-| The types of categorisation of questions.
-}
type Filter
    = FilterSystem
    | FilterStage
    | FilterTopic


searchRequest : Model -> Cmd Msg
searchRequest model =
    Request.getObjectiveList
        { auth = model.session.auth
        , systemFilters = CheckwordList.filterChecked model.systemDict
        , topicFilters = CheckwordList.filterChecked model.topicDict
        , stageFilters = CheckwordList.filterChecked model.stageDict
        , search = model.search
        , page = model.page
        , callback = GotSearchResults
        }


onSuccessAddedObjective : Objective.GetData -> Html msg
onSuccessAddedObjective objective =
    a
        [ Route.toHref (Route.Objective objective.id) ]
        [ text "Your objective was added successfully - thank you. Click here to navigate to it." ]


type Direction
    = Increment
    | Decrement
