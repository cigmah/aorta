module Update exposing (update)

import Question
import Types exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screen ) of
        ( UserClickedStart, StartScreen ) ->
            ( { model | screen = QuestionScreen NotResponded Question.default }, Cmd.none )

        _ ->
            ( model, Cmd.none )
