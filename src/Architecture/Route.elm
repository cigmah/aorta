module Architecture.Route exposing
    ( Route(..)
    , fromUrl
    , parser
    , toHref
    , toString
    )

import Architecture.Model exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | NotFound
    | Questions


{-| This parser is completely fragment-based to accommodate GitHub pages. |
-}
parser : Parser (Route -> a) a
parser =
    map fragmentToRoute <| fragment identity


fragmentToRoute : Maybe String -> Route
fragmentToRoute string =
    case string of
        Just "/questions" ->
            Questions

        Just "/" ->
            Home

        Just _ ->
            NotFound

        Nothing ->
            Home


fromUrl : Url -> Route
fromUrl url =
    parse parser url
        |> Maybe.withDefault NotFound


toString : Route -> String
toString route =
    let
        path =
            case route of
                Home ->
                    ""

                NotFound ->
                    "404"

                Questions ->
                    "questions"
    in
    "#/" ++ path


toHref : Route -> Attribute msg
toHref route =
    "./"
        ++ toString route
        |> href
