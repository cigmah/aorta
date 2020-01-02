module Page.ObjectiveList exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| An learning objective listing page, under `/objectives`.

This page is for viewing and searching a list of learning objectives.

-}

import Browser exposing (Document)
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
import Types.Specialty as Specialty exposing (Specialty)
import Types.Stage as Stage exposing (Stage)
import Types.Topic as Topic exposing (Topic)



-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------


{-| The page `Model` type.
-}
type alias Model =
    { session : Session
    , errors : Errors
    , specialtyDict : Dict Int (CheckwordData Msg)
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
    | ClickedSpecialty Int Bool -- toggled specialty filter
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
    | ChangedAddObjectiveSpecialty String -- change the new objective specialty (only authenticated)
    | ChangedAddObjectiveTopic String -- change the new objective topic (only authenticated)
    | ClickedAddObjective -- submit a new objective
    | GotAddObjectiveResponse (WebData Objective.GetData) -- receive the create-objective response



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


searchRequest : Model -> Cmd Msg
searchRequest model =
    Request.getObjectiveList
        { auth = model.session.auth
        , specialtyFilters = CheckwordList.filterChecked model.specialtyDict
        , topicFilters = CheckwordList.filterChecked model.topicDict
        , stageFilters = CheckwordList.filterChecked model.stageDict
        , search = model.search
        , page = model.page
        , callback = GotSearchResults
        }


{-| The page initialisation function.
-}
init : Session -> ( Model, Cmd Msg )
init session =
    let
        model =
            { session = session
            , errors = defaultErrors
            , specialtyDict = defaultSpecialtyDict
            , topicDict = defaultTopicDict
            , stageDict = defaultStageDict
            , results = Loading
            , filtersVisible = False
            , search = ""
            , page = 1
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

        ClickedSpecialty specialty bool ->
            model.specialtyDict
                |> Dict.update specialty (Maybe.map (updateCheckword bool))
                |> updateSpecialtyDict model
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
                FilterSpecialty ->
                    model.specialtyDict
                        |> selectAll
                        |> updateSpecialtyDict model
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
                FilterSpecialty ->
                    model.specialtyDict
                        |> deselectAll
                        |> updateSpecialtyDict model
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

        ChangedAddObjectiveSpecialty specialty ->
            specialty
                |> String.toInt
                |> Maybe.withDefault 0
                |> Specialty.enumerable.fromInt
                |> updateAddObjectiveSpecialty model
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
                |> withCmd (searchRequest model)

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
                { label = "Specialty(s)"
                , onSelectAll = ClickedSelectAll FilterSpecialty
                , onDeselectAll = ClickedDeselectAll FilterSpecialty
                , dict = model.specialtyDict
                , direction = Vertical
                }
            , CheckwordList.element
                { label = "Topic(s)"
                , onSelectAll = ClickedSelectAll FilterTopic
                , onDeselectAll = ClickedDeselectAll FilterTopic
                , dict = model.topicDict
                , direction = Vertical
                }
            , PrimaryButton.element
                { text = "Apply"
                , onClick = ClickedSearch
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
                                { label = "Specialty"
                                , value = model.addObjective.specialty
                                , enumerable = Specialty.enumerable
                                , onInput = ChangedAddObjectiveSpecialty
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


{-| A default specialty dictionary mapping specialty ints to checkword data.
-}
defaultSpecialtyDict : Dict Int (CheckwordData Msg)
defaultSpecialtyDict =
    CheckwordList.defaultDictFromEnumerable Specialty.enumerable ClickedSpecialty


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


{-| Update the specialty dictionary with a new one in the model
-}
updateSpecialtyDict : Model -> Dict Int (CheckwordData Msg) -> Model
updateSpecialtyDict model updatedDict =
    { model | specialtyDict = updatedDict }


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


{-| Modifies the add objective specialty.
-}
updateAddObjectiveSpecialty : Model -> Specialty -> Model
updateAddObjectiveSpecialty model specialty =
    { model | addObjective = model.addObjective |> Objective.updateSpecialty specialty }


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
    { model | addObjectiveResponse = response }


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
        newModel =
            case direction of
                Increment ->
                    { model | page = model.page + 1 }

                Decrement ->
                    { model | page = model.page - 1 }
    in
    ( newModel, searchRequest newModel )


{-| The types of categorisation of questions.
-}
type Filter
    = FilterSpecialty
    | FilterStage
    | FilterTopic


type Direction
    = Increment
    | Decrement
