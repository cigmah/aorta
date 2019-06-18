module Architecture.Route exposing
    ( Route(..)
    , fromUrl
    , parser
    , toString
    )

import Architecture.Model exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | NotFound
    | Classic


{-| This parser is completely fragment-based to accommodate GitHub pages. |
-}
parser : Parser (Route -> a) a
parser =
    map fragmentToRoute <| fragment identity


fragmentToRoute : Maybe String -> Route
fragmentToRoute string =
    case string of
        Just "/classic" ->
            Classic

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

                Classic ->
                    "classic"
    in
    "#/" ++ path
