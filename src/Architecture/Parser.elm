module Architecture.Parser exposing
    ( fromUrl
    , isEqual
    , parser
    )

import Architecture.Model as Model exposing (Model)
import Architecture.Route as Route exposing (Route)
import Maybe.Extra
import Url exposing (Url)
import Url.Parser exposing (..)
import Url.Parser.Query as Query


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Route.Home top
        , map Route.Profile <| s "profile"
        , map Route.Note <| s "notes" </> int
        , map Route.Revise <| s "revise"
        , map Route.Finish <| s "finish"
        , map Route.Question <| s "questions" </> int
        ]


queryFuture : Query.Parser (List Int)
queryFuture =
    let
        mapWithDefault f list =
            List.map f list
                |> Maybe.Extra.combine
                |> Maybe.withDefault []
    in
    Query.custom "future" (mapWithDefault String.toInt)


queryBack : Query.Parser (Maybe String)
queryBack =
    Query.string "back"


fromUrl : Url -> Route
fromUrl url =
    url
        |> parse parser
        |> Maybe.withDefault Route.NotFound


isEqual : Route -> Model -> Bool
isEqual route model =
    case ( route, model ) of
        ( Route.Home, Model.Home _ ) ->
            True

        ( Route.Profile, Model.Profile _ ) ->
            True

        ( Route.Note _, Model.Note _ ) ->
            True

        ( Route.Revise, Model.Revise _ ) ->
            True

        ( Route.Question _, Model.Question _ ) ->
            True

        ( Route.Finish, Model.Finish _ ) ->
            True

        _ ->
            False
