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
    | Profile
    | Revise
    | Note Int


{-| This parser is completely fragment-based to accommodate GitHub pages. |
-}
parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Profile <| s "profile"
        , map Note <| s "notes" </> int
        , map Revise <| s "revise"
        , map Home top
        ]


fromUrl : Url -> Route
fromUrl url =
    -- Using hash routing at the moment, so moving the fragment into the path.
    { url
        | path = Maybe.withDefault "" url.fragment
        , fragment = Nothing
    }
        |> parse parser
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

                Profile ->
                    "profile"

                Note noteId ->
                    "note/" ++ String.fromInt noteId

                Revise ->
                    "revise/"
    in
    "#/" ++ path


toHref : Route -> Attribute msg
toHref route =
    "./"
        ++ toString route
        |> href
