module Types exposing (Model, Msg(..), QuestionPhase(..), Screen(..), initialModel)

import Dict exposing (Dict)
import Question exposing (..)
import Set exposing (Set)
import Time exposing (Posix)


type Screen
    = StartScreen
    | QuestionScreen QuestionPhase Question


type QuestionPhase
    = NotResponded
    | Responded Choice


type alias Model =
    { screen : Screen }


type Msg
    = UserClickedStart
    | UserClickedResponse Choice
    | UserClickedNextQuestion



-- Defaults


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { screen = StartScreen }, Cmd.none )
