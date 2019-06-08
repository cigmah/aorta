module Update exposing (update)

import Question exposing (..)
import Random
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.screen of
        StartScreen ->
            updateStartScreen msg model

        LoadingScreen ->
            updateLoading msg model

        QuestionScreen questionPhase question ->
            updateQuestionScreen msg model questionPhase question



-- By Screen


updateStartScreen : Msg -> Model -> ( Model, Cmd Msg )
updateStartScreen msg model =
    case msg of
        UserClickedStart ->
            ( { model | screen = LoadingScreen }
            , Random.generate SystemGotShuffledQuestion (shuffleChoices Question.default)
            )

        _ ->
            ( model, Cmd.none )


updateLoading : Msg -> Model -> ( Model, Cmd Msg )
updateLoading msg model =
    case msg of
        SystemGotShuffledQuestion questionView ->
            ( { model | screen = QuestionScreen NotResponded questionView }, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateQuestionScreen : Msg -> Model -> QuestionPhase -> QuestionView -> ( Model, Cmd Msg )
updateQuestionScreen msg model questionPhase question =
    case questionPhase of
        NotResponded ->
            case msg of
                UserClickedResponse choice ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Responded choice ->
            case msg of
                UserClickedNextQuestion ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- Helpers
