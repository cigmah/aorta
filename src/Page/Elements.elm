module Page.Elements exposing
    ( SearchBarData
    , container
    , errorMessage
    , label
    , loadingGrid
    , messages
    , navBar
    , noteGrid
    , safeCenter
    , safeMain
    , searchBar
    , textInput
    , wrapError
    )

import Architecture.Route as Route exposing (Route)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (Error(..))
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import Types.Note as Note
import Types.Session as Session exposing (Session)
import Types.Specialty as Specialty
import Types.Styles exposing (tailwind)
import Types.Topic as Topic



-- Elements
-- Navigation


type alias NavLinkData =
    { name : String
    , active : Bool
    , route : Route
    , icon : String
    , hideOnMobile : Bool
    , hideOnDesktop : Bool
    }


navLink : Bool -> NavLinkData -> Html msg
navLink isRight data =
    a
        [ Route.toHref data.route
        , tailwind
            [ "p-2"
            , "px-4"
            , "flex-grow"
            , "flex-col"
            , "items-center"
            , "md:flex-none"
            , "md:flex-row"
            , "justify-center"
            , "cursor-pointer"
            , "hover:bg-gray-200"
            , "bg-white"
            , "hover:text-gray-700"
            , "text-xs"
            , "uppercase"
            , "outline-none"
            , "focus:bg-gray-300"
            , "font-bold"
            ]
        , classList
            [ ( "text-blue-500", data.active )
            , ( "md:ml-auto", isRight )
            , ( "hidden", data.hideOnMobile )
            , ( "flex", not data.hideOnMobile )
            , ( "md:hidden", data.hideOnDesktop )
            , ( "md:flex", not data.hideOnDesktop )
            ]
        ]
        [ i
            [ class "material-icons"
            , tailwind
                [ "md:pr-2" ]
            ]
            [ text data.icon ]
        , Html.label
            [ tailwind
                [ "cursor-pointer"
                , "md:pr-2"
                ]
            ]
            [ text data.name ]
        ]


navBar : Bool -> List NavLinkData -> SearchBarData Note.ListData msg -> List NavLinkData -> Html msg
navBar hideNav navLinksLeft searchBarData navLinksRight =
    nav
        [ tailwind
            [ "w-full"
            , "flex"
            , "fixed"
            , "bottom-0"
            , "md:top-0"
            , "md:bottom-auto"
            , "text-sm"
            , "md:text-base"
            , "items-stretch"
            , "z-30"
            , "border-b"
            , "border-gray-300"
            , "text-gray-500"
            , "bg-white"
            ]
        , classList
            [ ( "hidden", hideNav ) ]
        ]
        (div
            [ tailwind
                [ "hidden"
                , "ml-8"
                , "mr-6"
                , "text-xl"
                , "font-light"
                , "text-blue-500"
                , "md:flex"
                , "items-center"
                ]
            ]
            [ div [] [ text "aorta" ] ]
            :: List.map (navLink False) navLinksLeft
            ++ [ searchBar searchBarData ]
            ++ List.map (navLink True) navLinksRight
        )



-- Searchbar


type alias SearchBarData a msg =
    { webDataResponse : WebData (List a)
    , responseDataToResult : a -> Html msg
    , inputData : TextInputData msg
    , forMobile : Bool
    }


searchBar : SearchBarData a msg -> Html msg
searchBar data =
    div
        [ tailwind
            [ "flex-grow"
            , "m-2"
            , "relative"
            ]
        , classList
            [ ( "hidden md:flex", not data.forMobile )
            , ( "flex md:hidden", data.forMobile )
            ]
        ]
        [ textInput data.inputData
        , div
            [ tailwind
                [ "absolute"
                , "mt-12"
                , "bg-white"
                , "overflow-auto"
                , "shadow-lg"
                , "rounded"
                , "w-full"
                , "mb-16"
                , "md:mb-0"
                ]
            ]
            [ searchResults data.webDataResponse data.responseDataToResult ]
        ]


searchResults : WebData (List a) -> (a -> Html msg) -> Html msg
searchResults webData toResult =
    let
        wrap =
            div
                [ tailwind
                    [ "p-2"
                    , "md:h-64"
                    ]
                ]
    in
    case webData of
        Success data ->
            case data of
                [] ->
                    wrap [ div [ tailwind [ "p-2" ] ] [ text "No results" ] ]

                _ ->
                    wrap
                        (List.map toResult data)

        Failure e ->
            case e of
                Http.BadStatus code ->
                    wrap [ text <| "There was an error with error code " ++ String.fromInt code ]

                Http.NetworkError ->
                    wrap [ text <| "There was a network connectivity error." ]

                _ ->
                    wrap [ text "There was an error with searching. Try again later. " ]

        Loading ->
            wrap [ div [ tailwind [ "p-2" ] ] [ text "Loading" ] ]

        NotAsked ->
            div [] []



-- Messages


singleMessage : (String -> msg) -> String -> Html msg
singleMessage clickedMessage string =
    article
        [ class "message"
        , tailwind
            [ "bg-white"
            , "text-gray-800"
            , "p-4"
            , "fadein"
            , "shadow-lg"
            , "md:rounded"
            , "md:mb-4"
            , "cursor-pointer"
            , "flex"
            , "justify-between"
            ]
        , onClick <| clickedMessage string
        ]
        [ div [] (Markdown.toHtml Nothing string)
        , div [ class "material-icons" ] [ text "clear" ]
        ]


messages : Session -> (String -> msg) -> Html msg
messages session clickedMessage =
    case session.message of
        Just stringList ->
            section
                [ class "message-list"
                , tailwind
                    [ "fixed"
                    , "right-0"
                    , "top-0"
                    , "md:p-2"
                    , "md:w-1/4"
                    , "z-50"
                    , "w-full"
                    ]
                ]
                (List.map (singleMessage clickedMessage) stringList)

        Nothing ->
            section
                [ class "hidden" ]
                []



-- Pages


{-| A main that puts enough padding on the bottom for mobile navigation.
-}
safeMain : List (Html msg) -> Html msg
safeMain =
    main_
        [ tailwind
            [ "p-2"
            , "md:p-4"
            , "pb-16"
            , "md:pb-4"
            , "md:min-h-screen"
            , "md:pt-16"
            , "h-screen"
            , "overflow-auto"
            , "md:h-auto"
            ]
        ]


safeCenter : List (Html msg) -> Html msg
safeCenter =
    main_
        [ tailwind
            [ "p-4"
            , "pb-16"
            , "h-screen"
            , "md:h-auto"
            , "md:min-h-screen"
            , "md:flex"
            , "justify-center"
            , "items-center"
            , "overflow-auto"
            , "md:pb-4"
            , "md:pt-16"
            ]
        ]


container : List (Html msg) -> Html msg
container =
    section
        [ tailwind
            [ "container"
            , "mx-auto"
            , "min-h-full"
            , "md:flex"
            ]
        ]



-- Inputs


type alias TextInputData msg =
    { value : String
    , onInput : String -> msg
    , placeholder : String
    , type_ : String
    }


textInput : TextInputData msg -> Html msg
textInput data =
    input
        [ type_ data.type_
        , value data.value
        , onInput data.onInput
        , placeholder data.placeholder
        , tailwind
            [ "outline-none"
            ]
        ]
        []



-- Messages


errorMessage : Html msg -> Html msg
errorMessage message =
    article
        [ tailwind
            [ "max-w-3xl"
            , "mx-auto"
            , "p-4"
            , "md:shadow-lg"
            , "md:border-t-4"
            , "md:border-red-600"
            , "rounded-b"
            ]
        ]
        [ header
            [ tailwind
                [ "uppercase"
                , "text-red-600"
                , "font-bold"
                , "font-sm"
                , "pb-2"
                , "border-b"
                , "border-red-300"
                ]
            ]
            [ text "Pulseless Electrical Activity" ]
        , section
            [ tailwind
                [ "my-2" ]
            , class "markdown"
            ]
            [ div [ tailwind [ "flex" ] ]
                [ p [ tailwind [ "mr-1" ] ]
                    [ i [ class "material-icons", tailwind [ "mr-2", "flex", "text-red-500" ] ] [ text "error" ] ]
                , p [ tailwind [ "flex-grow" ] ]
                    [ strong [] [ text "Pulseless electrical activity (PEA)" ]
                    , text " is when an ECG shows a rhythm, but there's no pulse to be felt."
                    ]
                ]
            , p []
                [ text "We had an error. We're sorry. Here's some extra details on the error: " ]
            , div
                [ tailwind
                    [ "bg-red-200"
                    , "text-red-700"
                    , "font-medium"
                    , "rounded"
                    , "m-4"
                    , "px-2"
                    , "py-1"
                    ]
                ]
                [ message ]
            , p []
                [ text "Let us know if this persists!" ]
            ]
        ]


wrapError : Http.Error -> Html msg
wrapError error =
    let
        message =
            case error of
                Http.BadBody string ->
                    div
                        []
                        [ p [] [ text "There was a bad request return body. Here's the error dump for debugging: " ]
                        , p [ tailwind [ "text-xs" ] ] [ text string ]
                        ]

                Http.BadStatus code ->
                    case code of
                        401 ->
                            p [] [ text "You weren't authorised to make this request (code 401)." ]

                        404 ->
                            p [] [ text "The destination wasn't found (code 404)." ]

                        500 ->
                            p [] [ text "Ah...there was a dreaded internal server error (code 500). Let us know! This shouldn't happen." ]

                        _ ->
                            p [] [ text <| "Error code " ++ String.fromInt code ]

                Http.NetworkError ->
                    p [] [ text "There was a network error - is your internet connection working? If it's working, our server might be down. Try again later." ]

                Http.Timeout ->
                    p [] [ text "The request timed out. Try again later!" ]

                Http.BadUrl string ->
                    p []
                        [ text "That url was faulty. Here's some extra information:"
                        , p [] [ text string ]
                        ]
    in
    errorMessage message



-- Note Grid


noteTailwind : Attribute msg
noteTailwind =
    tailwind
        [ "md:w-6"
        , "md:h-6"
        , "lg:w-8"
        , "lg:h-8"
        , "my-px"
        , "w-full"
        , "md:m-px"
        , "rounded"
        , "relative"
        , "p-2"
        , "md:p-0"
        , "transition"
        , "min-w-0"
        ]


gridHeaderTailwind : Attribute msg
gridHeaderTailwind =
    tailwind
        [ "text-xs"
        , "text-gray-700"
        , "flex"
        , "items-center"
        , "hidden"
        , "transition"
        , "md:flex"
        , "min-w-0"
        ]


gridRowHeaders : Bool -> List (Html msg)
gridRowHeaders loading =
    let
        showHeader row specialty =
            div
                [ style "grid-column" "1"
                , style "grid-row" (String.fromInt <| row + 2)
                , gridHeaderTailwind
                , tailwind [ "text-right", "pr-3", "justify-end" ]
                , classList [ ( "opacity-25", loading ) ]
                ]
                [ text (Specialty.toString specialty) ]
    in
    List.indexedMap showHeader Specialty.list


gridColumnHeaders : Bool -> List (Html msg)
gridColumnHeaders loading =
    let
        showHeader column topic =
            div
                [ style "grid-column" (String.fromInt <| column + 2)
                , style "grid-row" "1"
                , tailwind [ "w-8", "leading-tight", "pb-4" ]
                , gridHeaderTailwind
                , classList [ ( "opacity-25", loading ) ]
                ]
                [ div
                    [ tailwind [ "rotate-90", "min-w-0", "text-left" ] ]
                    [ text (Topic.toBrief topic) ]
                ]
    in
    List.indexedMap showHeader Topic.list


viewLoadingItem : Int -> Int -> Html msg
viewLoadingItem column row =
    a
        [ noteTailwind
        , tailwind [ "fadeinout", "bg-gray-100" ]
        , style "grid-column" (String.fromInt <| column + 2)
        , style "grid-row" (String.fromInt <| row + 2)
        ]
        []


viewGridItem : Note.ListData -> Html msg
viewGridItem note =
    a
        [ class "note"
        , noteTailwind
        , style "background" (Color.toCssString <| Note.toBgColor note)
        , style "border" ("1px solid " ++ (Color.toCssString <| Note.toBorderColor note))
        , Route.toHref (Route.Note note.id)
        , attribute "data-tooltip" note.title
        , style "grid-column" (String.fromInt (Topic.toInt note.topic + 2))
        , style "grid-row" (String.fromInt (Specialty.toInt note.specialty + 2))
        ]
        []


grid : List (Html msg) -> Html msg
grid contents =
    article
        [ tailwind
            [ "hidden"
            , "md:grid"
            , "mx-auto"
            , "w-auto"
            ]
        ]
        contents


noteGrid : List Note.ListData -> Html msg
noteGrid data =
    grid <|
        gridRowHeaders False
            ++ gridColumnHeaders False
            ++ List.map viewGridItem data


loadingGrid : Html msg
loadingGrid =
    grid <|
        gridRowHeaders True
            ++ gridColumnHeaders True
            ++ (List.map viewLoadingItem (List.range 0 (List.length Topic.list - 1))
                    |> List.map (\f -> List.map f (List.range 0 (List.length Specialty.list - 1)))
                    |> List.concat
               )



-- Forms


label : String -> String -> Html msg
label labelText forId =
    Html.label
        [ tailwind
            [ "font-bold"
            , "text-gray-600"
            , "text-xs"
            , "uppercase"
            ]
        , for forId
        ]
        [ text labelText ]
