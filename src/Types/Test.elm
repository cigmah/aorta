module Types.Test exposing (Test)

import Types.Choice as Choice
import Types.Comment as Comment
import Types.Question as Question


type alias Test =
    { completed :
        List
            { id : Int
            , wasCorrect : Bool
            }
    , future : List Int
    , back : String
    }
