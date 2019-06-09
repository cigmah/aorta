module Architecture.Route exposing (Route(..), fromUrl, parser)

import Architecture.Model exposing (..)
import Architecture.Msg exposing (..)
import Browser.Navigation exposing (Key, replaceUrl)
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Home
    | NotFound


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top ]


fromUrl : Url -> Route
fromUrl url =
    parse parser url
        |> Maybe.withDefault NotFound
