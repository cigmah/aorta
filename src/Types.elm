module Types exposing (Model, Msg(..), QuestionPhase(..), Screen(..), hasResponded, initialModel)

import Dict exposing (Dict)
import Question exposing (..)
import Set exposing (Set)
import Time exposing (Posix)


type Screen
    = StartScreen
    | LoadingScreen
    | QuestionScreen QuestionPhase QuestionView


type QuestionPhase
    = NotResponded
    | Responded ChoiceView


type alias Model =
    { screen : Screen }


type Msg
    = UserClickedStart
    | SystemGotShuffledQuestion QuestionView
    | UserClickedResponse ChoiceView
    | UserClickedNextQuestion



-- Functions


hasResponded : QuestionPhase -> Bool
hasResponded questionPhase =
    case questionPhase of
        NotResponded ->
            False

        Responded _ ->
            True



-- Defaults


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { screen = StartScreen }, Cmd.none )
