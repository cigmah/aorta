module Types.Styles exposing (tailwind)

import Html exposing (Attribute)
import Html.Attributes exposing (class)


tailwind : List String -> Attribute msg
tailwind classes =
    classes
        |> String.join " "
        |> class
