module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| AORTA's home page, under `/` or `/index.html`.

This is the homepage that users will see when they visit the root URL. As a
principle, there should be minimal ceremony or friction to starting a
multiple choice test. Login should not be necessary, nor focal on the
homepage.

-}

import Browser exposing (Document)
import Dict exposing (Dict)
import Element.BackgroundImage as BackgroundImage
import Element.CheckwordList as CheckwordList exposing (CheckwordData, Direction(..))
import Element.LandingFloat as LandingFloat
import Element.PrimaryButton as PrimaryButton
import Element.TwoColumn as TwoColumn
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import RemoteData exposing (RemoteData(..), WebData)
import TypedSvg
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Icon as Icon
import Types.Interface exposing (Enumerable)
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Stage as Stage exposing (Stage)
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
    , specialtyDict : Dict Int (CheckwordData Msg)
    , topicDict : Dict Int (CheckwordData Msg)
    , stageDict : Dict Int (CheckwordData Msg)
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
    | ClickedSpecialty Int Bool
    | ClickedTopic Int Bool
    | ClickedStage Int Bool
    | ClickedSelectAll Filter
    | ClickedDeselectAll Filter



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


{-| The page initialisation function.
-}
init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , errors = defaultErrors
      , specialtyDict = defaultSpecialtyDict
      , topicDict = defaultTopicDict
      , stageDict = defaultStageDict
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
            [ CheckwordList.element
                { label = "Year Levels"
                , onSelectAll = ClickedSelectAll FilterStage
                , onDeselectAll = ClickedDeselectAll FilterStage
                , dict = model.stageDict
                , direction = Horizontal
                }
            , TwoColumn.element
                { first =
                    CheckwordList.element
                        { label = "Specialties"
                        , onSelectAll = ClickedSelectAll FilterSpecialty
                        , onDeselectAll = ClickedDeselectAll FilterSpecialty
                        , dict = model.specialtyDict
                        , direction = Vertical
                        }
                , second =
                    CheckwordList.element
                        { label = "Topics"
                        , onSelectAll = ClickedSelectAll FilterTopic
                        , onDeselectAll = ClickedDeselectAll FilterTopic
                        , dict = model.topicDict
                        , direction = Vertical
                        }
                }
            , PrimaryButton.element
                { text = "Start Questions"
                , onClick = NoOp
                , submit = False
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
        |> deselectAll


{-| Updates a checkword's `checked` value with a provided boolean.
-}
updateCheckword : Bool -> CheckwordData msg -> CheckwordData msg
updateCheckword newBool checkword =
    { checkword | checked = newBool }


{-| Update all dict values to checked = True.
-}
selectAll : Dict Int (CheckwordData msg) -> Dict Int (CheckwordData msg)
selectAll =
    Dict.map (\_ value -> updateCheckword True value)


{-| Update all dict values to checked = False.
-}
deselectAll : Dict Int (CheckwordData msg) -> Dict Int (CheckwordData msg)
deselectAll =
    Dict.map (\_ value -> updateCheckword False value)


{-| Wrap a model in no side effect command. A convenience helper.
-}
withCmdNone : Model -> ( Model, Cmd Msg )
withCmdNone model =
    ( model, Cmd.none )


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


{-| The types of categorisation of questions.
-}
type Filter
    = FilterSpecialty
    | FilterStage
    | FilterTopic
