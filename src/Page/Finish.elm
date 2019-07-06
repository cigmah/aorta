module Page.Finish exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)
import Types.Test as Test exposing (Test)



-- Model


type alias Model =
    { session : Session
    , lastTest : Maybe Test
    }



-- Msg


type Msg
    = NoOp
    | ClickedFinish



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        newSession =
            { session | test = Nothing }
    in
    ( { session = newSession
      , lastTest = session.test
      }
    , Cmd.none
    )



-- Eject


eject : Model -> Session
eject model =
    model.session



-- Inject


inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ session, lastTest } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedFinish ->
            case lastTest of
                Just test ->
                    ( model
                    , Navigation.pushUrl
                        session.key
                        test.back
                    )

                Nothing ->
                    ( model
                    , Navigation.pushUrl
                        session.key
                        (Route.toString Route.Home)
                    )



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Finished!"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ section [ class "modal" ]
        [ viewTestResults model ]
    ]


tailwindButton =
    tailwind
        [ "border-2"
        , "bg-white"
        , "hover:bg-blue-500"
        , "hover:text-white"
        , "border-blue-500"
        , "text-sm"
        , "uppercase"
        , "font-bold"
        ]


viewTestResults : Model -> Html Msg
viewTestResults model =
    case model.lastTest of
        Just test ->
            case test.future of
                [] ->
                    article
                        []
                        [ header [] [ div [ tailwind [ "text-center", "w-full", "text-lg", "py-4" ] ] [ text "DONE!" ] ]
                        , viewContentBody test
                        , footer []
                            [ button [ onClick ClickedFinish, tailwindButton ] [ text "Finish" ] ]
                        ]

                _ ->
                    article []
                        [ section [] [ text "How did you get here? You shouldn't be here. You're still doing a test." ]
                        , footer []
                            [ button
                                [ tailwindButton
                                , onClick ClickedFinish
                                ]
                                [ text "Go Home" ]
                            ]
                        ]

        Nothing ->
            article
                []
                [ section [] [ text "It doesn't seem like you were doing a test! Go home?" ]
                , footer []
                    [ button
                        [ tailwindButton
                        , onClick ClickedFinish
                        ]
                        [ text "Go Home" ]
                    ]
                ]


viewContentBody : Test -> Html Msg
viewContentBody test =
    let
        numQuestions =
            List.length test.completed

        numCorrect =
            List.filter .wasCorrect test.completed
                |> List.length

        percentCorrect =
            case numQuestions of
                0 ->
                    0

                _ ->
                    round (toFloat numCorrect / toFloat numQuestions * 100)

        ( textColor, bgColor, borderColor ) =
            if percentCorrect >= 75 then
                ( "text-green-500", "bg-green-500", "border-green-500" )

            else if percentCorrect >= 50 then
                ( "text-orange-500", "bg-orange-500", "border-orange-500" )

            else
                ( "text-red-500", "bg-red-500", "border-red-500" )

        viewCompleted item =
            let
                ( tailwindStyles, icon ) =
                    if item.wasCorrect then
                        ( tailwind [ "border-green-500", "text-green-500", "hover:bg-green-500" ]
                        , span [ class "material-icons", tailwind [ "mr-2" ] ] [ text "done" ]
                        )

                    else
                        ( tailwind [ "border-red-500", "text-red-500", "hover:bg-red-500" ]
                        , span [ class "material-icons", tailwind [ "mr-2" ] ] [ text "clear" ]
                        )
            in
            a
                [ tailwind
                    [ "flex"
                    , "items-center"
                    , "uppercase"
                    , "p-2"
                    , "text-sm"
                    , "border-2"
                    , "rounded"
                    , "font-bold"
                    , "m-2"
                    , "hover:text-white"
                    ]
                , tailwindStyles
                , Route.toHref (Route.Question item.id)
                ]
                [ icon
                , text <| "Question ID " ++ String.fromInt item.id
                ]
    in
    section [ tailwind [ "flex", "flex-col", "items-center", "justify-start" ] ]
        [ div
            [ tailwind
                [ "mx-auto"
                , textColor
                , "font-bold"
                , "text-5xl"
                , "p-4"
                , "border-8"
                , borderColor
                , "rounded-full"
                , "fadein"
                , "h-48"
                , "w-48"
                , "flex"
                , "justify-center"
                , "items-center"
                ]
            ]
            [ text <| String.fromInt percentCorrect ++ "%" ]
        , h1
            [ tailwind
                [ "mt-8"
                , "border-t"
                , "border-gray-300"
                , "font-bold"
                , "text-gray-600"
                , "uppercase"
                , "text-sm"
                , "w-full"
                , "text-center"
                , "pt-2"
                ]
            ]
            [ text "Seen Questions" ]
        , p [ tailwind [ "my-2", "text-gray-700", "text-left", "px-2" ] ] [ text "If you would like to review these questions, open them in a new tab. Your test session will be restarted once you leave this page." ]
        , div [ tailwind [ "flex", "flex-wrap", "mt-2" ] ]
            (List.map viewCompleted test.completed)
        ]
