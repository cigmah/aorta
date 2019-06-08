module Question exposing (Bank, ChoiceBase, ChoiceView, Flag(..), QuestionBase, QuestionId, QuestionTruth, QuestionView, Tag(..), choiceDescription, correctToChoiceView, default, questionTruthToView, shuffleChoices)

import Dict exposing (Dict)
import Random exposing (Generator)
import Random.List
import Set exposing (Set)
import Time exposing (Posix)


type alias QuestionBase a =
    { a
        | stem : String
        , explanation : String
        , createdBy : Maybe String
        , lastRevised : Posix
        , tags : Set Tag
        , flags : Set Flag
    }


type alias QuestionTruth =
    QuestionBase
        { correctChoice : ChoiceBase {}
        , incorrectChoiceList : List (ChoiceBase {})
        }


type alias QuestionView =
    QuestionBase { choices : List ChoiceView }


type alias QuestionId =
    Int


type alias Bank =
    Dict QuestionId QuestionTruth


type Tag
    = Gp
    | Obgyn
    | Psych
    | Paeds


type Flag
    = NoShow


type alias ChoiceBase a =
    { a | description : String, explanation : String }


type alias ChoiceView =
    ChoiceBase { isCorrect : Bool }



-- Functions


choiceDescription : ChoiceBase a -> String
choiceDescription choice =
    choice.description


correctToChoiceView : Bool -> ChoiceBase {} -> ChoiceView
correctToChoiceView isCorrect choiceBase =
    { description = choiceBase.description
    , explanation = choiceBase.explanation
    , isCorrect = isCorrect
    }


questionTruthToView : QuestionTruth -> List ChoiceView -> QuestionView
questionTruthToView q choices =
    { stem = q.stem
    , explanation = q.explanation
    , createdBy = q.createdBy
    , lastRevised = q.lastRevised
    , tags = q.tags
    , flags = q.flags
    , choices = choices
    }


shuffleChoices : QuestionTruth -> Generator QuestionView
shuffleChoices question =
    correctToChoiceView True question.correctChoice
        :: List.map (correctToChoiceView False) question.incorrectChoiceList
        |> Random.List.shuffle
        |> Random.map (questionTruthToView question)



-- Default


default =
    { stem = "This is a **test** of Markdown!"
    , correctChoice =
        { description = "This is the actual *right* answer."
        , explanation = "This is incorrect because..."
        }
    , incorrectChoiceList =
        [ { description = "This is *another* test of Markdown."
          , explanation = "This is correct because..."
          }
        ]
    , explanation = "What a measly explanation"
    , createdBy = Nothing
    , lastRevised = Time.millisToPosix 0
    , tags = Set.empty
    , flags = Set.empty
    }
