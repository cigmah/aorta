module Architecture.Route exposing
    ( Route(..)
    , toHref
    , toString
    )

{-| Contains the basic `Route` type.

The `Route` type is a discriminated union which enumerates all the possible
routes of the application. It can be thought of as a "middleman" between the
URL and the `Model` type, which allows some extra information from the URL to
be added to each individual page's initialisation and ensures the routing
logic is fairly straightforward.

-}

import Html exposing (Attribute)
import Html.Attributes exposing (href)
import Url.Builder as Builder


{-| The basic application `Route` type.

All possible pages of the application need to be enumerated here, with any
extra information needed by a page's initialisation function appended to its
variant. When a URL is parsed and mapped to a route, it can then pass the
information straight to here, which can then be mapped to the page
initialisation function.

-}
type Route
    = Home
    | NotFound
    | Profile
    | ObjectiveList
    | Objective Int
    | Question Int
    | Report


{-| Converts a `Route` into a string URL path. }

This is useful for adding type-safe links to pages. These need to be the same
as the strings listed in the `parser` to work.

-}
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

        Objective objectiveId ->
            Builder.absolute
                [ "objectives"
                , String.fromInt objectiveId
                ]
                []

        ObjectiveList ->
            Builder.absolute
                [ "objectives" ]
                []

        Question questionId ->
            Builder.absolute
                [ "questions"
                , String.fromInt questionId
                ]
                []

        Report ->
            Builder.absolute
                [ "report" ]
                []


{-| Converts a `Route` to an HTML Attribute `href`.

This can be used to create type-safe `href` attributes.

-}
toHref : Route -> Attribute msg
toHref route =
    ""
        ++ toString route
        |> href
