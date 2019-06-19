module Page.Questions exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Choice as Choice exposing (Choice)
import Types.Question as Question exposing (Question)
import Types.Request as Request exposing (GetRequest, PostRequest)
import Types.Session as Session exposing (Session)


type State
    = Filtering Filters
    | Loading
    | Randomising Question
    | Presenting Question Data
    | Correct Question Data
    | Incorrect Choice Question Data
    | Finished Data


type alias Filters =
    {}


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
    | ClickedStart
    | GotQuestionList (WebData (List Question))


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , state = Filtering {}
      , debug = Nothing
      }
    , Cmd.none
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

        ClickedStart ->
            -- TODO
            let
                request =
                    { endpoint = Request.GetQuestionRandom
                    , returnDecoder = Decode.list Question.decoder
                    , callback = GotQuestionList
                    , auth = model.session.auth
                    }
            in
            ( model, Cmd.none )

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
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    case model.state of
        Filtering filters ->
            viewFiltering model filters

        Loading ->
            []

        Randomising question ->
            []

        Presenting question data ->
            []

        Correct question data ->
            []

        Incorrect choice question data ->
            []

        Finished data ->
            []


type ChoiceStatus
    = Neutral
    | SelectedIncorrect
    | SelectedCorrect


viewChoice : { a | content : String, explanation : String, percentChosen : Int, status : ChoiceStatus } -> Html Msg
viewChoice data =
    let
        ( baseColours, headerColours ) =
            case data.status of
                Neutral ->
                    ( "bg-gray-100 hover:bg-gray-300 hover:text-gray-900", "bg-gray-200" )

                SelectedCorrect ->
                    ( "bg-green-200 text-white", "bg-green-500" )

                SelectedIncorrect ->
                    ( "bg-red-200 text-white", "bg-red-500" )
    in
    li
        []
        [ details
            [ class "rounded my-1 text-gray-900"
            , classList [ ( baseColours, True ) ]
            ]
            [ summary
                [ class "flex items-center m-1 py-1 px-4 rounded-t"
                , classList [ ( headerColours, True ) ]
                ]
                [ span [] [ text data.content ]
                , span
                    [ class "flex-grow text-right" ]
                    [ text <| String.fromInt data.percentChosen ++ "%" ]
                ]
            , p
                [ class "px-4 py-2 font-normal text-sm" ]
                (Markdown.toHtml Nothing data.explanation)
            ]
        ]


viewTag : { a | content : String } -> Html Msg
viewTag data =
    li
        [ class "rounded-full bg-gray-300 px-2 mx-1 text-xs text-gray-500" ]
        [ text data.content ]


viewQuestion : { q | choiceList : List { a | content : String, explanation : String, percentChosen : Int, status : ChoiceStatus }, tagList : List { t | content : String } } -> Html Msg
viewQuestion data =
    article
        [ class "sm:m-8 max-w-xl sm:shadow-lg rounded mt-10 mb-8 relative bg-white" ]
        [ header
            [ class "rounded-t hidden w-full bg-gray-200 py-2 sm:px-2 text-sm font-bold text-gray-600 sm:flex justify-between" ]
            [ ul
                [ class "flex overflow-auto" ]
                (List.map viewTag data.tagList)
            , p
                [ class "bg-gray-200 px-2" ]
                [ text "1/10" ]
            ]
        , section
            [ class "bg-white pb-12 sm:pb-12" ]
            [ p
                [ class "text-gray-800 font-medium m-4" ]
                [ text "Test question body." ]
            , section
                []
                [ ol
                    [ class "m-4 text-gray-800 font-medium" ]
                    (List.map viewChoice data.choiceList)
                ]
            ]
        , footer
            [ class "fixed h-16 sm:h-auto sm:absolute bg-green-500 p-1 left-0 bottom-0 w-full text-white font-bold rounded-b" ]
            [ section
                [ class "flex justify-between items-center h-full" ]
                [ p
                    [ class "p-1 px-4" ]
                    [ text "Great job!" ]
                , button
                    [ class "p-1 px-3 bg-green-600 shaddow-inner rounded hover:bg-green-700 font-bold h-full" ]
                    [ text "Next Question" ]
                ]
            ]
        ]


viewFiltering : Model -> Filters -> List (Html Msg)
viewFiltering model filters =
    [ section
        [ class "h-screen bg-gray-800 text-white flex justify-center items-center flex w-full sm:bg-white sm:shadow" ]
        [ article
            []
            [ button [ class "bg-gray-200 text-gray-700 px-2 rounded py-1 shadow-inner hover:bg-gray-400 text-gray-900" ] [ text "Start Questions" ]
            , button [ class "bg-gray-200 text-gray-700 px-2 rounded py-1 shadow-inner hover:bg-gray-400 text-gray-900" ] [ text "Create Questions" ]
            ]
        ]
    ]
