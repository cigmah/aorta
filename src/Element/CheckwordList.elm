module Element.CheckwordList exposing (..)

{-| A list of checkboxes, except the words act as the click and display target.

Functions primarily as a multi-select.

-}

import Dict as Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types.Icon as Icon
import Types.Interface exposing (Enumerable)


{-| A single checkword item.

The values held by items are Ints, so the type (usually an enum) must support
an operation to convert it into intgers.

-}
type alias CheckwordData msg =
    { word : String
    , value : Int
    , checked : Bool
    , onCheck : Bool -> msg
    }


{-| Converts a string to a string without spaces and all lower-case.
-}
toId : String -> String
toId string =
    string
        |> String.toLower
        |> String.replace " " ""


{-| A single rendered checkword item.
-}
checkword : CheckwordData msg -> Html msg
checkword data =
    let
        checkwordId =
            "checkword-checkbox-" ++ toId data.word
    in
    li [ class "checkword" ]
        [ input
            [ class "checkword-checkbox"
            , id checkwordId
            , type_ "checkbox"
            , checked data.checked
            , onCheck data.onCheck
            ]
            []
        , label
            [ class "checkword-label"
            , tabindex 0
            , for checkwordId
            ]
            [ text data.word ]
        ]


{-| Converts an enumerable and a message into a default checkword item.
-}
defaultCheckwordFromEnumerable : Enumerable a -> (Int -> Bool -> msg) -> a -> CheckwordData msg
defaultCheckwordFromEnumerable enumerable toMsg item =
    { word = enumerable.toBriefString item
    , checked = False
    , value = enumerable.toInt item
    , onCheck = toMsg (enumerable.toInt item)
    }


{-| Converts an enumerable into a default dictionary of checkwords with checked = True.
-}
defaultDictFromEnumerable : Enumerable a -> (Int -> Bool -> msg) -> Dict Int (CheckwordData msg)
defaultDictFromEnumerable enumerable toMsg =
    let
        toKeyValue item =
            ( enumerable.toInt item, defaultCheckwordFromEnumerable enumerable toMsg item )
    in
    enumerable.list
        |> List.map toKeyValue
        |> Dict.fromList


type Direction
    = Horizontal
    | Vertical


directionToClass : Direction -> String
directionToClass direction =
    case direction of
        Horizontal ->
            "horizontal"

        Vertical ->
            "vertical"


{-| Data required for a full checkword list.
-}
type alias Data msg =
    { label : String
    , onSelectAll : msg
    , onDeselectAll : msg
    , dict : Dict Int (CheckwordData msg)
    , direction : Direction
    }


element : Data msg -> Html msg
element data =
    let
        directionClass =
            directionToClass data.direction
    in
    section
        [ class "checkword-list" ]
        [ div [ class "checkword-list-row" ]
            [ label
                [ class "checkword-list-label" ]
                [ text data.label ]
            , button
                [ class "checkword-list-select-all"
                , onClick data.onSelectAll
                , type_ "button"
                ]
                [ Icon.selectAll ]
            , button
                [ class "checkword-list-deselect-all"
                , onClick data.onDeselectAll
                , type_ "button"
                ]
                [ Icon.deselectAll ]
            ]
        , ul
            [ class "checkword-list-list"
            , classList [ ( directionClass, True ) ]
            ]
            (data.dict |> Dict.values |> List.sortBy .word |> List.map checkword)
        ]


{-| Updates a checkword's `checked` value with a provided boolean.
-}
updateCheckword : Bool -> CheckwordData msg -> CheckwordData msg
updateCheckword newBool data =
    { data | checked = newBool }


{-| Update all dict values to checked = True.
-}
selectAll : Dict Int (CheckwordData msg) -> Dict Int (CheckwordData msg)
selectAll =
    Dict.map (\_ value -> updateCheckword True value)


{-| Update all dict values to checked = False.
-}
deselectAll : Dict Int (CheckwordData msg) -> Dict Int (CheckwordData msg)
deselectAll =
    Dict.map (\_ value -> updateCheckword False value)


{-| Filters a dictionary of checkword data by checked values.
-}
filterChecked : Dict Int (CheckwordData msg) -> List Int
filterChecked dict =
    dict
        |> Dict.filter (\key value -> .checked value)
        |> Dict.keys
