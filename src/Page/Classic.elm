module Page.Classic exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice exposing (Choice)
import Types.Question as Question exposing (Question)
import Types.Request as Request exposing (GetRequest, PostRequest)
import Types.Session as Session exposing (Session)


type State
    = Loading
    | Randomising Question
    | Presenting Question Data
    | Correct Question Data
    | Incorrect Choice Question Data
    | Finished Data


type alias Data =
    { questionList : List Question
    , answeredList : List Question
    }


type alias Model =
    { session : Session
    , state : State
    , debug : Maybe (WebData (List Question))
    }


type Msg
    = NoOp
    | GotQuestionList (WebData (List Question))


init : Session -> ( Model, Cmd Msg )
init session =
    let
        request =
            { endpoint = Request.GetQuestionRandom
            , returnDecoder = Decode.list Question.decoder
            , callback = GotQuestionList
            , auth = session.auth
            }
    in
    ( { session = session
      , state = Loading
      , debug = Nothing
      }
    , Request.get request
    )


eject : Model -> Session
eject model =
    model.session


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        GotQuestionList questionListWebData ->
            case questionListWebData of
                Success questionList ->
                    let
                        firstQuestion =
                            List.head questionList
                    in
                    case firstQuestion of
                        Just question ->
                            ( { model | state = Randomising question }, Cmd.none )

                        Nothing ->
                            ( { model | state = Finished (Data [] []) }, Cmd.none )

                _ ->
                    ( { model
                        | debug = Just questionListWebData
                        , session = Session.addMessage model.session "There was an error!"
                      }
                    , Cmd.none
                    )


view : Model -> Document Msg
view model =
    { title = ""
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [] []
