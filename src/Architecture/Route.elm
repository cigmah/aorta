module Architecture.Route exposing
    ( Route(..)
    , toHref
    , toString
    )

import Browser.Navigation exposing (Key, replaceUrl)
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Url exposing (Url)
import Url.Builder as Builder


type Route
    = Home
    | NotFound
    | Profile
    | Revise
    | Note Int
    | Question Int
    | Info
    | Finish


buildFuture : List Int -> List Builder.QueryParameter
buildFuture future =
    future
        |> List.map (Builder.string "future" << String.fromInt)


toString : Route -> String
toString route =
    case route of
        Home ->
            Builder.absolute
                [ "" ]
                []

        NotFound ->
            Builder.absolute
                [ "404" ]
                []

        Profile ->
            Builder.absolute
                [ "profile" ]
                []

        Note noteId ->
            Builder.absolute
                [ "notes"
                , String.fromInt noteId
                ]
                []

        Revise ->
            Builder.absolute
                [ "revise" ]
                []

        Question questionId ->
            Builder.absolute
                [ "questions"
                , String.fromInt questionId
                ]
                []

        Finish ->
            Builder.absolute
                [ "finish" ]
                []

        Info ->
            Builder.absolute
                [ "information" ]
                []


toHref : Route -> Attribute msg
toHref route =
    ""
        ++ toString route
        |> href
