module Page.Home exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Architecture.Route as Route
import Browser exposing (Document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Note as Note
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty exposing (Specialty)
import Types.Styles exposing (tailwind)
import Types.Topic as Topic exposing (Topic)
import Types.YearLevel as YearLevel exposing (YearLevel)
import Url.Builder as Builder



-- Model


type alias Model =
    { session : Session
    , webDataNoteList : WebData (List Note.ListData)
    , filter : String
    , results : WebData (List Note.ListData)
    }



-- Msg


type Msg
    = NoOp
    | GotNoteList (WebData (List Note.ListData))
    | ChangedFilter String
    | ClearedFilter
    | GotResults (WebData (List Note.ListData))



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    let
        initialModel =
            { session = session
            , webDataNoteList = Loading
            , filter = ""
            , results = NotAsked
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
            -- At some point, may need to change this if it hits the backend too hard
            let
                cmd =
                    if String.length string > 3 then
                        Request.get (getFilteredNoteList model string)

                    else
                        Cmd.none

                results =
                    if String.length string > 3 then
                        Loading

                    else
                        NotAsked
            in
            ( { model | filter = string, results = results }, cmd )

        ClearedFilter ->
            ( { model | filter = "", results = NotAsked }, Cmd.none )

        GotResults results ->
            ( { model | results = results }, Cmd.none )



-- Requests


getNoteList : Model -> Request.GetRequest (List Note.ListData) Msg
getNoteList model =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotNoteList
    , returnDecoder = Note.decoderList
    , queryList = []
    }


getFilteredNoteList : Model -> String -> Request.GetRequest (List Note.ListData) Msg
getFilteredNoteList model string =
    { auth = model.session.auth
    , endpoint = Request.GetNoteList
    , callback = GotResults
    , returnDecoder = Note.decoderList
    , queryList = [ Builder.string "search" string ]
    }



-- View


view : Model -> Document Msg
view model =
    { title = "AORTA - Grid"
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ main_
        [ tailwind
            [ "pb-16"
            , "md:pb-0"
            ]
        ]
        [ section
            [ tailwind
                [ "w-full"
                , "md:flex"
                , "md:flex-col"
                , "md:pt-24"
                , "p-2"
                ]
            ]
            [ section [ tailwind [ "md:w-1/2", "mx-auto", "z-10", "hidden" ] ]
                [ input
                    [ value model.filter
                    , onInput ChangedFilter
                    , placeholder "Search matrix item titles here."
                    , tailwind [ "w-full", "text-lg", "p-2", "mx-auto" ]
                    ]
                    []
                , viewResults model.results
                ]
            , article
                [ tailwind
                    [ "md:container"
                    , "w-full"
                    , "md:mt-4"
                    ]
                ]
                [ section
                    [ tailwind
                        [ "flex"
                        , "relative"
                        ]
                    ]
                    [ viewGrid model.webDataNoteList ]
                ]
            ]
        ]
    ]


viewGrid : WebData (List Note.ListData) -> Html Msg
viewGrid webData =
    case webData of
        NotAsked ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "This is an error that shouldn't happen. If you see it, let us know!" ]

        Loading ->
            div [ tailwind [ "flex", "flex-col", "md:grid", "mx-auto" ] ]
                (gridRowHeaders True
                    ++ gridColumnHeaders True
                    ++ (List.map viewLoadingItem (List.range 0 (List.length Topic.list - 1))
                            |> List.map (\f -> List.map f (List.range 0 (List.length Specialty.list - 1)))
                            |> List.concat
                       )
                )

        Failure e ->
            div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                [ text "Hm, it seems like there was an issue. Try refreshing - if it persists, let us know!" ]

        Success listData ->
            case listData of
                [] ->
                    div [ tailwind [ "flex", "justify-center", "items-center", "flex-grow", "h-full" ] ]
                        [ text "There don't appear to be any notes." ]

                nonEmptyData ->
                    div
                        [ tailwind
                            [ "flex"
                            , "flex-col"
                            , "md:grid"
                            , "mx-auto"
                            ]
                        ]
                        (gridRowHeaders False ++ gridColumnHeaders False ++ List.map viewGridItem nonEmptyData)


gridRowHeaders : Bool -> List (Html Msg)
gridRowHeaders loading =
    let
        showHeader row specialty =
            div
                [ style "grid-column" "1"
                , style "grid-row" (String.fromInt <| row + 2)
                , tailwind
                    [ "text-xs"
                    , "text-gray-700"
                    , "text-right"
                    , "pr-3"
                    , "md:flex"
                    , "items-center"
                    , "justify-end"
                    , "hidden"
                    , "transition"
                    ]
                , classList [ ( "opacity-25", loading ) ]
                ]
                [ text (Specialty.toString specialty) ]
    in
    List.indexedMap showHeader Specialty.list


gridColumnHeaders : Bool -> List (Html Msg)
gridColumnHeaders loading =
    let
        showHeader column topic =
            div
                [ style "grid-column" (String.fromInt <| column + 2)
                , style "grid-row" "1"
                , tailwind
                    [ "text-xs"
                    , "text-gray-700"
                    , "w-8"
                    , "leading-tight"
                    , "md:flex"
                    , "items-center"
                    , "hidden"
                    , "pb-4"
                    , "transition"
                    ]
                , classList [ ( "opacity-25", loading ) ]
                ]
                [ div
                    [ tailwind [ "rotate-90", "min-w-0", "text-left" ] ]
                    [ text (Topic.toBrief topic) ]
                ]
    in
    List.indexedMap showHeader Topic.list


noteTailwind : Attribute Msg
noteTailwind =
    tailwind
        [ "md:w-6"
        , "md:h-6"
        , "lg:w-8"
        , "lg:h-8"
        , "my-1"
        , "md:m-px"
        , "rounded"
        , "relative"
        , "p-2"
        , "md:p-0"
        , "transition"
        ]


viewLoadingItem : Int -> Int -> Html Msg
viewLoadingItem column row =
    a
        [ noteTailwind
        , tailwind [ "fadeinout", "bg-gray-100" ]
        , style "grid-column" (String.fromInt <| column + 2)
        , style "grid-row" (String.fromInt <| row + 2)
        ]
        []


viewGridItem : Note.ListData -> Html Msg
viewGridItem note =
    a
        [ class "note"
        , noteTailwind
        , tailwind [ "bg-gray-200" ]
        , Route.toHref (Route.Note note.id)
        , attribute "data-tooltip" note.title
        , style "grid-column" (String.fromInt (Topic.toInt note.topic + 2))
        , style "grid-row" (String.fromInt (Specialty.toInt note.specialty + 2))
        ]
        [ div
            [ tailwind
                [ "flex", "md:hidden" ]
            ]
            [ div
                [ tailwind
                    [ "bg-gray-400"
                    , "h-24"
                    , "w-24"
                    , "mr-4"
                    , "rounded"
                    ]
                ]
                []
            , div []
                [ h1
                    [ tailwind
                        [ "text-gray-700"
                        , "uppercase"
                        , "font-bold"
                        , "mb-2"
                        ]
                    ]
                    [ text note.title ]
                , div [ tailwind [ "flex", "flex-wrap" ] ]
                    [ div
                        [ tailwind
                            [ "text-xs"
                            , "text-gray-600"
                            , "bg-gray-300"
                            , "px-2"
                            , "rounded-full"
                            , "mr-2"
                            , "mb-2"
                            ]
                        ]
                        [ text (note.specialty |> Specialty.toString) ]
                    , div
                        [ tailwind
                            [ "text-xs"
                            , "text-gray-600"
                            , "bg-gray-300"
                            , "px-2"
                            , "rounded-full"
                            , "mb-2"
                            ]
                        ]
                        [ text (note.topic |> Topic.toString) ]
                    ]
                , div
                    [ tailwind [ "flex", "flex-wrap" ] ]
                    [ div [ tailwind [ "text-xs", "mr-4" ] ] [ text (String.fromInt note.numQuestions ++ " questions.") ]
                    , div [ tailwind [ "text-xs" ] ] [ text (String.fromInt note.numComments ++ " contributions.") ]
                    ]
                ]
            ]
        ]


viewResults : WebData (List Note.ListData) -> Html Msg
viewResults webData =
    let
        viewResult : Note.ListData -> Html Msg
        viewResult note =
            article
                []
                [ text note.title ]
    in
    case webData of
        Success notes ->
            case notes of
                [] ->
                    section
                        [ tailwind
                            [ "bg-white", "p-2", "rounded-b", "absolute" ]
                        ]
                        [ article [] [ text "There are no results." ] ]

                _ ->
                    section
                        [ tailwind
                            [ "bg-white", "p-2", "rounded-b", "absolute" ]
                        ]
                        (List.map viewResult notes)

        Loading ->
            section
                [ tailwind
                    [ "bg-white", "p-2", "rounded-b", "absolute" ]
                ]
                [ article [] [ text "Loading" ] ]

        Failure e ->
            section
                [ tailwind [ "bg-white", "p-2", "rounded-b" ] ]
                [ article [] [ text "There was an error" ] ]

        NotAsked ->
            section [] []
