module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder



-- Model


type alias Model =
    { session : Session
    , yearLevel : YearLevel
    , webDataNoteList : WebData (List Note.ListData)
    , filter : String
    }



-- Msg


type Msg
    = NoOp
    | GotNoteList (WebData (List Note.ListData))
    | ChangedYearLevel YearLevel
    | ChangedFilter String



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , yearLevel = session.yearLevel
            , webDataNoteList = Loading
            , filter = ""
            }
    in
    ( initialModel
    , Request.get (getNoteList initialModel)
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
update msg model =
    let
        ignore =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            ignore

        GotNoteList webData ->
            ( { model | webDataNoteList = webData }, Cmd.none )

        ChangedFilter string ->
            ( { model | filter = string }, Cmd.none )

        ChangedYearLevel yearLevel ->
            if model.yearLevel /= yearLevel then
                let
                    newSession =
                        Session.changeYearLevel model.session yearLevel

                    newModel =
                        { model
                            | yearLevel = yearLevel
                            , webDataNoteList = Loading
                            , session = newSession
                            , filter = ""
                        }
                in
                ( newModel
                , Cmd.batch
                    [ Request.get (getNoteList newModel)
                    , Session.save newSession
                    ]
                )

            else
                ignore



-- Requests


getNoteList : Model -> Request.GetRequest (List Note.ListData) Msg
getNoteList model =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    , queryList = [ Builder.int "year_level" (YearLevel.toInt model.yearLevel) ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Matrix"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_
        [ tailwind
            [ "bg-gray-100"
            , "min-h-screen"
            ]
        ]
        [ section
            [ tailwind
                [ "md:h-screen"
                , "md:bg-gray-300"
                , "w-full"
                , "flex"
                , "flex-col"
                , "justify-center"
                , "items-center"
                , "md:p-4"
                ]
            ]
            [ article
                [ tailwind
                    [ "md:container"
                    , "md:shadow-xl"
                    , "bg-white"
                    , "md:rounded-b-lg"
                    , "overflow-hidden"
                    , "md:m-16"
                    , "md:h-full"
                    , "w-full"
                    , "md:w-2/3"
                    ]
                ]
                [ viewHeader model
                , section
                    [ tailwind
                        [ "md:flex"
                        , "items-start"
                        , "md:h-full"
                        , "overflow-auto"
                        , "relative"
                        , "hidden"
                        ]
                    ]
                    [ viewGrid model.webDataNoteList ]
                ]
            ]
        , section
            [ tailwind
                [ "container"
                , "mx-auto"
                , "my-4"
                , "flex"
                , "justify-center"
                , "p-3"
                ]
            ]
            [ input
                [ value model.filter
                , onInput ChangedFilter
                , placeholder "Search matrix item titles here."
                , tailwind [ "md:w-1/2" ]
                ]
                []
            ]
        , section
            [ tailwind
                [ "container"
                , "mx-auto"
                , "pb-32"
                , "px-3"
                , "flex"
                , "flex-col"
                , "sm:flex-row"
                , "sm:flex-wrap"
                , "md:pt-8"
                , "sm:justify-center"
                , "md:p-8"
                , "min-h-screen"
                ]
            ]
            (viewCards model.filter model.webDataNoteList)
        ]
    ]


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ tailwind
            [ "flex"
            , "sticky"
            , "md:relative"
            ]
        ]
        (List.map
            (viewHeaderItem model.yearLevel)
            [ YearLevel.Year1, YearLevel.Year2a, YearLevel.Year3b, YearLevel.Year4c ]
        )


viewHeaderItem : YearLevel -> YearLevel -> Html Msg
viewHeaderItem active yearLevel =
    div
        [ tailwind
            [ "text-center"
            , "flex-grow"
            , "py-2"
            , "px-4"
            , "hover:bg-white"
            , "hover:text-gray-600"
            , "cursor-pointer"
            ]
        , classList
            [ ( "bg-white text-gray-600", active == yearLevel )
            , ( "bg-gray-600 text-white", not (active == yearLevel) )
            ]
        , onClick (ChangedYearLevel yearLevel)
        ]
        [ text (YearLevel.toString yearLevel) ]


viewGrid : WebData (List Note.ListData) -> Html Msg
viewGrid webData =
    case webData of
        NotAsked ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "Not asked" ]

        Loading ->
            div
                [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ div [ class "loading" ] [] ]

        Failure e ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "Failure" ]

        Success listData ->
            case listData of
                [] ->
                    div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                        [ text "Oh no! There aren't any notes for this year level yet..." ]

                nonEmptyData ->
                    div
                        [ tailwind
                            [ "flex"
                            , "flex-wrap"
                            , "m-8"
                            , "fadein"
                            ]
                        ]
                        (List.map viewGridItem nonEmptyData)


viewGridItem : Note.ListData -> Html Msg
viewGridItem note =
    let
        baseColor =
            Specialty.toColor note.specialty

        darkerColor =
            Specialty.toDark note.specialty

        ( initial, _ ) =
            note.title
                |> String.uncons
                |> Maybe.withDefault ( ' ', "" )
    in
    a
        [ class "note"
        , tailwind
            [ "w-8"
            , "h-8"
            , "m-px"
            , "rounded"
            , "flex"
            , "justify-center"
            , "items-center"
            ]
        , href ("#/notes/" ++ String.fromInt note.id)
        , Html.Attributes.attribute "data-tooltip" note.title
        , Html.Attributes.style "background" (note.specialty |> Specialty.toColor |> Color.toCssString)
        , Html.Attributes.style "color" (darkerColor |> Color.toCssString)
        ]
        [ text (String.fromChar initial) ]


viewCards : String -> WebData (List Note.ListData) -> List (Html Msg)
viewCards filter webData =
    case webData of
        Loading ->
            [ div [ class "loading" ] [] ]

        NotAsked ->
            []

        Failure e ->
            []

        Success data ->
            data
                |> List.filter (\item -> String.contains (String.toLower filter) (String.toLower item.title))
                |> List.map viewCard


viewCard : Note.ListData -> Html Msg
viewCard note =
    a
        [ href ("#/notes/" ++ String.fromInt note.id)
        , tailwind [ "my-1", "sm:m-2" ]
        ]
        [ article
            [ class "card"
            , tailwind
                [ "text-white"
                , "md:h-40"
                , "w-full"
                , "sm:w-64"
                , "md:w-40"
                , "rounded-lg"
                , "shadow-md"
                , "flex"
                , "flex-col"
                , "cursor-pointer"
                , "hover:shadow-xl"
                , "overflow-hidden"
                , "fadein"
                ]
            , Html.Attributes.style "background" (note.specialty |> Specialty.toMedium |> Color.toCssString)
            ]
            [ section
                [ tailwind
                    [ "font-sm"
                    , "p-2"
                    , "flex-grow"
                    , "font-bold"
                    ]
                ]
                [ text note.title ]
            , footer
                [ tailwind
                    [ "flex"
                    , "p-1"
                    , "flex-wrap"
                    ]
                ]
                [ div [ class "tag" ] [ text (Specialty.toString note.specialty) ]
                , div [ class "tag" ] [ text (YearLevel.toString note.yearLevel) ]
                , div [ class "tag" ] [ text (String.fromInt note.numComments ++ " comments") ]
                , div [ class "tag" ] [ text (String.fromInt note.numQuestions ++ " questions") ]
                ]
            ]
        ]
