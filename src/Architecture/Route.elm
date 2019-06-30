module Architecture.Route exposing
    ( Route(..)
    , fromUrl
    , isEqual
    , parser
    , toHref
    , toString
    )

import Architecture.Model as Model exposing (..)
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
    | Question Int
    | Finish


{-| This parser is completely fragment-based to accommodate GitHub pages. |
-}
parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Profile <| s "profile"
        , map Note <| s "notes" </> int
        , map Question <| s "questions" </> int
        , map Revise <| s "revise"
        , map Finish <| s "finish"
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
                    "404/"

                Profile ->
                    "profile/"

                Note noteId ->
                    "note/" ++ String.fromInt noteId

                Revise ->
                    "revise/"

                Question questionId ->
                    "question/" ++ String.fromInt questionId

                Finish ->
                    "finish/"
    in
    "#/" ++ path


toHref : Route -> Attribute msg
toHref route =
    ""
        ++ toString route
        |> href


isEqual : Route -> Model -> Bool
isEqual route model =
    case ( route, model ) of
        ( Home, Model.Home _ ) ->
            True

        ( Profile, Model.Profile _ ) ->
            True

        ( Note _, Model.Note _ ) ->
            True

        ( Revise, Model.Revise _ ) ->
            True

        ( Question _, Model.Question _ ) ->
            True

        ( Finish, Model.Finish _ ) ->
            True

        _ ->
            False
