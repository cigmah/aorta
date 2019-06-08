module Question exposing (Bank, BaseChoice, Choice(..), Flag(..), Question, QuestionId, Tag(..), default)

import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Posix)


type alias Question =
    { stem : String
    , choices : List Choice
    , explanation : String
    , createdBy : Maybe String
    , lastRevised : Posix
    , tags : Set Tag
    , flags : Set Flag
    }


type alias QuestionId =
    Int


type alias Bank =
    Dict QuestionId Question


type Tag
    = Gp
    | Obgyn
    | Psych
    | Paeds


type Flag
    = NoShow


type Choice
    = CorrectChoice (BaseChoice { whyCorrect : String })
    | IncorrectChoice (BaseChoice { whyIncorrect : String })


type alias BaseChoice a =
    { a
        | description : String
        , isCorrect : Bool
    }



-- Default


default =
    { stem = "This is a **test** of Markdown!"
    , choices =
        [ IncorrectChoice
            { description = "This is *another* test of Markdown."
            , isCorrect = False
            , whyIncorrect = "This is correct because..."
            }
        , CorrectChoice
            { description = "This is the actual *right* answer."
            , isCorrect = True
            , whyCorrect = "This is incorrect because..."
            }
        ]
    , explanation = "What a measly explanation"
    , createdBy = Nothing
    , lastRevised = Time.millisToPosix 0
    , tags = Set.empty
    , flags = Set.empty
    }
