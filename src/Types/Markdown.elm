module Types.Markdown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown


options : Markdown.Options
options =
    { githubFlavored = Just { tables = True, breaks = False }
    , defaultHighlighting = Nothing
    , sanitize = True
    , smartypants = False
    }


markdown : String -> Html msg
markdown =
    Markdown.toHtmlWith options [ class "markdown" ]
