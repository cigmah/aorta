module Architecture.Parser exposing
    ( fromUrl
    , isEqual
    )

{-| Contains the basic URL parser for the application.

When the application is navigated to, the URL contains information relating
to which page should be loaded, and possibly some extra information for that
page. The parser in this module is responsible for parsing the URL and
mapping it to the correct variant of the `Route` discriminated union, which
is then mapped to the proper page's initialisation function.

-}

import Architecture.Model as Model exposing (Model)
import Architecture.Route as Route exposing (Route)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, top)


{-| A Parser to map URL paths to a `Route`.

Any possible URL for the application should be listed here with the `Route`
that it maps to. Information that needs to be passed from the URL into the
initialised model needs to be part of the `Route` variant, so the parser can
pass that information straightaway to the `Route`.

-}
parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Route.Home top
        , map Route.Home <| s "index.html"
        , map Route.Profile <| s "profile"
        , map Route.Objective <| s "objectives" </> int
        , map Route.ObjectiveList <| s "objectives"
        , map Route.Report <| s "report"
        , map Route.Question <| s "questions" </> int
        ]


{-| Converts a URL to a `Route`, with a default `Route.NotFound`.

When the application URL changes, this uses the `parser` function to map the
URL into a new `Route` (which can then be mapped to a new model using the
corresponding page's initialisation function).

-}
fromUrl : Url -> Route
fromUrl url =
    url
        |> parse parser
        |> Maybe.withDefault Route.NotFound


{-| Checks whether `Model` is currently on a specified `Route`.

This may be useful for links which should appear different if the user is
already on the linked-to page.

-}
isEqual : Route -> Model -> Bool
isEqual route model =
    case ( route, model ) of
        ( Route.Home, Model.Home _ ) ->
            True

        ( Route.Profile, Model.Profile _ ) ->
            True

        ( Route.Objective _, Model.Objective _ ) ->
            True

        ( Route.ObjectiveList, Model.ObjectiveList _ ) ->
            True

        ( Route.Question _, Model.Question _ ) ->
            True

        ( Route.Report, Model.Report _ ) ->
            True

        _ ->
            False
