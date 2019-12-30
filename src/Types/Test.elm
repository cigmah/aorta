module Types.Test exposing (Test)


type alias Test =
    { completed :
        List
            { id : Int
            , wasCorrect : Bool
            }
    , future : List Int
    , back : String
    }
