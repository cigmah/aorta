module Page.Question exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

{-| This page is the main EMQ review page for reviewing questions.

This page may be entered by:

1.  Clicking Start Questions from the Home Page
2.  Clicking a question from the parent objective page
3.  Clicking Next Question from this page itself
4.  Accessing a URL e.g. ./questions/1/

-}

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Element.QuestionDetail as QuestionDetail
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Page.Utils as Utils exposing (withCmd, withCmdNone)
import Random
import Random.List
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice
import Types.Comment as Comment
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Question as Question
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Test as Test



-------------------------------------------------------------------------------
-- MODEL
-------------------------------------------------------------------------------


{-| The page `Model` type.
-}
type alias Model =
    { session : Session
    , questionId : Int
    , randomisedChoices : Maybe (List Choice.GetData)
    , questionWebData : WebData Question.GetDetailData
    , selected : Maybe Choice.GetData
    , responseResponse : WebData ()
    , rating : Maybe Int
    , ratingResponse : WebData ()
    , comment : String
    , commentResponse : WebData Comment.GetData
    , showObjective : Bool
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
    | GotQuestion (WebData Question.GetDetailData)
    | GotRandomisedChoices (List Choice.GetData)
    | ClickedReload
    | ClickedChoice Int
    | GotQuestionResponseResponse (WebData ())
    | ClickedRating Int
    | GotQuestionRatingResponse (WebData ())
    | ChangedComment String
    | ClickedSubmitComment
    | GotQuestionCommentResponse (WebData Comment.GetData)
    | ToggledShowObjective
    | ClickedNextQuestion
    | ClickedExit



-------------------------------------------------------------------------------
-- INIT
-------------------------------------------------------------------------------


{-| The page initialisation function.
-}
init : Session -> Int -> ( Model, Cmd Msg )
init session questionId =
    let
        model =
            { session = session
            , errors = defaultErrors
            , questionId = questionId
            , randomisedChoices = Nothing
            , questionWebData = Loading
            , responseResponse = NotAsked
            , selected = Nothing
            , rating = Nothing
            , ratingResponse = NotAsked
            , comment = ""
            , commentResponse = NotAsked
            , showObjective = False
            }
    in
    ( model, requestQuestion model )



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

        GotQuestion response ->
            -- if the response was successful, then shuffle the choices
            case response of
                Success question ->
                    model
                        |> updateQuestionWebData response
                        |> withCmd
                            (question.choices
                                |> Dict.values
                                |> Random.List.shuffle
                                |> Random.generate GotRandomisedChoices
                            )

                _ ->
                    model
                        |> updateQuestionWebData response
                        |> withCmdNone

        GotRandomisedChoices choices ->
            model
                |> updateRandomisedChoices choices
                |> withCmdNone

        ClickedReload ->
            model
                |> updateQuestionWebData Loading
                |> withCmd (requestQuestion model)

        ClickedChoice key ->
            model
                |> updateSelected key
                |> updateResponseResponse Loading
                |> withCmd (requestPostResponse key model)

        GotQuestionResponseResponse response ->
            model
                |> updateResponseResponse response
                |> withCmdNone

        -- the key corresponds to the choice ID anyway
        ClickedRating rating ->
            model
                |> updateRating rating
                |> updateRatingResponse Loading
                |> withCmd (requestPostRating rating model)

        GotQuestionRatingResponse response ->
            model
                |> updateRatingResponse response
                |> withCmdNone

        ChangedComment comment ->
            model
                |> updateComment comment
                |> withCmdNone

        ClickedSubmitComment ->
            model
                |> updateCommentResponse Loading
                |> withCmd (requestPostComment model)

        -- if the comment was received successfully, add it to the list of comments and reset
        GotQuestionCommentResponse response ->
            case response of
                Success newComment ->
                    model
                        |> addComment newComment
                        |> updateComment ""
                        |> updateCommentResponse response
                        |> withCmdNone

                _ ->
                    model
                        |> updateCommentResponse response
                        |> withCmdNone

        ToggledShowObjective ->
            model
                |> toggleShowObjective
                |> withCmdNone

        ClickedExit ->
            model
                |> goBack

        -- when the user clicks next question, it depends on whether they were doing a test or not
        ClickedNextQuestion ->
            case model.session.test of
                -- if they were doing a test, then proceed to the next question or the report
                Just test ->
                    -- these criteria must be present for this event to be
                    case ( model.questionWebData, model.selected ) of
                        ( Success question, Just selected ) ->
                            let
                                completed =
                                    { question = question
                                    , choice = selected
                                    }

                                ( newTest, route ) =
                                    Test.addCompleted completed test
                            in
                            ( { model | session = Session.updateTest newTest model.session }
                            , Navigation.pushUrl model.session.key (Route.toString route)
                            )

                        -- this is an impossible state, indicating they haven't done the question yet
                        _ ->
                            ( model, Cmd.none )

                -- if they weren't doing a test, then reroute them follow the same procedure as clicking exit
                Nothing ->
                    model
                        |> goBack



-------------------------------------------------------------------------------
-- VIEW
-------------------------------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        title =
            case model.questionWebData of
                Success question ->
                    "Question ID " ++ String.fromInt question.id

                _ ->
                    "Loading Question"
    in
    { title = title
    , body = viewBody model
    }


{-| Render the page body from an immutable model view.
-}
viewBody : Model -> List (Html Msg)
viewBody model =
    [ QuestionDetail.element
        { questionWebData = model.questionWebData
        , test = model.session.test
        , randomisedChoices = model.randomisedChoices
        , selected = model.selected
        , rating = model.rating
        , comment = model.comment
        , showObjective = model.showObjective
        , onClickReload = ClickedReload
        , onClickChoice = ClickedChoice
        , responseWebData = model.responseResponse
        , onClickRating = ClickedRating
        , ratingWebData = model.ratingResponse
        , onChangeComment = ChangedComment
        , onClickSubmitComment = ClickedSubmitComment
        , commentWebData = model.commentResponse
        , onToggleShowObjective = ToggledShowObjective
        , onClickExit = ClickedExit
        , onClickNextQuestion = ClickedNextQuestion
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


{-| Requests the question by ID.
-}
requestQuestion : Model -> Cmd Msg
requestQuestion model =
    Request.getQuestionDetail
        { auth = model.session.auth
        , questionId = model.questionId
        , callback = GotQuestion
        }


requestPostResponse : Int -> Model -> Cmd Msg
requestPostResponse key model =
    Request.postQuestionResponse
        { auth = model.session.auth
        , callback = GotQuestionResponseResponse
        , choiceId = key
        }


requestPostRating : Int -> Model -> Cmd Msg
requestPostRating rating model =
    Request.postQuestionRating
        { auth = model.session.auth
        , callback = GotQuestionRatingResponse
        , rating = rating
        , questionId = model.questionId
        }


requestPostComment : Model -> Cmd Msg
requestPostComment model =
    Request.postQuestionComment
        { auth = model.session.auth
        , callback = GotQuestionCommentResponse
        , comment = model.comment
        , questionId = model.questionId
        }


{-| Selects a question choice.
-}
updateSelected : Int -> Model -> Model
updateSelected key model =
    case model.questionWebData of
        Success question ->
            { model | selected = Dict.get key question.choices }

        _ ->
            model


updateRandomisedChoices : List Choice.GetData -> Model -> Model
updateRandomisedChoices choices model =
    { model | randomisedChoices = Just choices }


{-| Sorry for the confusing name. Might refactor.

Updates the web response to a question response.

-}
updateResponseResponse : WebData () -> Model -> Model
updateResponseResponse webData model =
    { model | responseResponse = webData }


updateRating : Int -> Model -> Model
updateRating rating model =
    { model | rating = Just rating }


updateRatingResponse : WebData () -> Model -> Model
updateRatingResponse webData model =
    { model | ratingResponse = webData }


updateComment : String -> Model -> Model
updateComment comment model =
    { model | comment = comment }


updateCommentResponse : WebData Comment.GetData -> Model -> Model
updateCommentResponse webData model =
    { model | commentResponse = webData }


toggleShowObjective : Model -> Model
toggleShowObjective model =
    { model | showObjective = not model.showObjective }


goBack : Model -> ( Model, Cmd Msg )
goBack model =
    case model.session.back of
        -- if there was a "back" registered in the route, then go back and set it to Nothing again
        Just route ->
            let
                newSession =
                    model.session
                        |> Session.setBack Nothing
                        |> Session.clearTest
            in
            ( { model | session = newSession }
            , Navigation.pushUrl newSession.key (Route.toString route)
            )

        -- Otherwise, the user came from the url or the home page, so reroute them back to the Home page
        Nothing ->
            let
                newSession =
                    model.session
                        |> Session.clearTest
            in
            ( { model | session = newSession }
            , Navigation.pushUrl model.session.key (Route.toString Route.Home)
            )


addComment : Comment.GetData -> Model -> Model
addComment newComment model =
    case model.questionWebData of
        Success question ->
            { model | questionWebData = Success { question | comments = List.append question.comments [ newComment ] } }

        _ ->
            model


updateQuestionWebData : WebData Question.GetDetailData -> Model -> Model
updateQuestionWebData response model =
    { model | questionWebData = response }
