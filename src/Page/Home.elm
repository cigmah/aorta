module Page.Home exposing (Model, Msg, eject, init, subscriptions, update, view)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Secret exposing (baseUrl)
import Types.Credentials exposing (Auth(..))
import Types.Question
import Types.Request
import Types.Session exposing (Session)
import Types.User as User


type alias Model =
    { session : Session
    , stats : Maybe User.Stats
    }


type Msg
    = NoOp
    | ClickedClassicMode
    | ClickedAdventureMode


init : Session -> ( Model, Cmd Msg )
init session =
    case session.auth of
        Guest ->
            ( { session = session
              , stats = Nothing
              }
            , Cmd.none
            )

        User credentials ->
            -- TODO Request stats
            ( { session = session
              , stats = Nothing
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

        ClickedClassicMode ->
            ( model, Navigation.pushUrl model.session.key "/#/classic" )

        ClickedAdventureMode ->
            -- TODO
            ignore


view : Model -> Document Msg
view model =
    { title = "AORTA - Pinboard"
    , body = viewBody model
    }


markdown : String -> List (Html Msg)
markdown content =
    Markdown.toHtml Nothing content


viewCard : { title : String, content : String } -> Html Msg
viewCard data =
    article
        [ class "bg-white rounded shadow m-2 w-auto max-w-2xl sm:w-full sm:mx-auto" ]
        [ header [ class "bg-gray-200 text-gray-600 px-2 py-2" ]
            [ p [ class "font-bold" ] [ text data.title ] ]
        , section
            [ class "p-2 text-gray-900" ]
            (markdown data.content)
        , footer [] []
        ]


viewInfo : Html Msg
viewInfo =
    article
        [ class "h-screen flex flex-col justify-center bg-gray-800 px-4 sm:px-0 sm:block sm:h-auto sm:bg-white sm:rounded sm:shadow w-full max-w-2xl mx-auto" ]
        [ header [ class "text-3xl sm:text-xl bg-gray-800 text-white px-2 sm:rounded-t pt-8 pb-2 sm:pt-2" ]
            [ p [ class "font-bold" ] [ text "AORTA" ] ]
        , section
            [ class "p-2 text-white sm:text-gray-900" ]
            [ strong [] [ text "AORTA" ]
            , text " is "
            , strong [] [ text "An Open Revision Tool for Assessments" ]
            , text "; a free and open-source revision tool built by the "
            , a [ class "underline", href "https://cigmah.github.io/" ] [ text "Coding Interest Group in Medicine and Healthcare" ]
            , text ". Updates to this tool will be posted in this space; users are also free to post feedback, feature requests or anything else of public interest here."
            ]
        ]


tests =
    List.repeat 20 { title = "test", content = "This *is* some content." }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ section [ class "sm:mt-16 sm:px-1 sm:mb-2" ] [ viewInfo ]
    , hr [ class "hidden sm:block sm:my-4" ] []
    , section [ class "flex flex-col w-full mb-16 sm:px-1" ]
        (List.map viewCard tests)
    ]
