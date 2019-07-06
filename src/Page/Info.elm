module Page.Info exposing (Model, Msg, eject, init, inject, subscriptions, update, view)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown
import Page.Elements as Elements
import RemoteData exposing (RemoteData(..), WebData)
import Types.Contact as Contact
import Types.Credentials as Credentials exposing (Auth(..))
import Types.Request as Request
import Types.Session as Session exposing (Session)
import Types.Styles exposing (tailwind)



-- Model


type alias Model =
    { session : Session
    , contactData : Contact.Data
    , contactResponse : WebData Bool
    }



-- Msg


type Msg
    = NoOp
    | ContactMsg ContactSubMsg


type ContactSubMsg
    = ContactChangedName String
    | ContactChangedEmail String
    | ContactChangedSubject String
    | ContactChangedBody String
    | ContactClickedSubmit
    | ContactGotSubmissionResponse (WebData Bool)
    | ContactNoOp



-- Init


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , contactData = Contact.init
      , contactResponse = NotAsked
      }
    , Cmd.none
    )



-- Eject


eject : Model -> Session
eject model =
    model.session



-- Inject


inject : Model -> Session -> ( Model, Cmd Msg )
inject model session =
    ( { model | session = session }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ContactMsg contactSubMsg ->
            updateContact contactSubMsg model


updateContact : ContactSubMsg -> Model -> ( Model, Cmd Msg )
updateContact msg ({ contactData, session } as model) =
    case msg of
        ContactChangedName value ->
            ( { model | contactData = { contactData | name = value } }, Cmd.none )

        ContactChangedEmail value ->
            ( { model | contactData = { contactData | email = value } }, Cmd.none )

        ContactChangedSubject value ->
            ( { model | contactData = { contactData | subject = value } }, Cmd.none )

        ContactChangedBody value ->
            ( { model | contactData = { contactData | body = value } }, Cmd.none )

        ContactClickedSubmit ->
            if contactData.loading then
                ( model, Cmd.none )

            else
                ( { model | contactData = { contactData | loading = True } }
                , Request.post (postContactData model) |> Cmd.map ContactMsg
                )

        ContactGotSubmissionResponse responseWebData ->
            let
                unloaded =
                    { model | contactData = { contactData | loading = False } }

                cleared =
                    { model | contactData = Contact.init }

                addMessage message =
                    Session.addMessage unloaded.session message

                withMessage newModel message =
                    { newModel | session = addMessage message, contactResponse = responseWebData }
            in
            case responseWebData of
                Success _ ->
                    ( withMessage cleared "Your message was received. Thank you. We will attend to it as soon as possible."
                    , Cmd.none
                    )

                Failure error ->
                    ( withMessage unloaded "There was an error with sending your message. We apologise for the inconvenience."
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ContactNoOp ->
            ( model, Cmd.none )



-- Requests


postContactData : Model -> Request.PostRequest Bool ContactSubMsg
postContactData model =
    { endpoint = Request.PostContact
    , body = Contact.encode model.contactData
    , returnDecoder = Contact.responseDecoder
    , callback = ContactGotSubmissionResponse
    , auth = model.session.auth
    , queryList = []
    }



-- View


view : Model -> Document Msg
view model =
    { title = ""
    , body = viewBody model
    }


viewBody : Model -> List (Html Msg)
viewBody model =
    [ Elements.safeMain
        [ Elements.container
            [ section [ tailwind [] ] [ cardInformation ]
            , section [ tailwind [] ] [ cardContact model |> Html.map ContactMsg ]
            ]
        ]
    ]


submitContent : Model -> String
submitContent model =
    if model.contactData.loading then
        "Loading"

    else
        "Submit"


cardInformation : Html Msg
cardInformation =
    Elements.articleCard
        { header = text "FAQ"
        , body = div [ class "markdown" ] (Markdown.toHtml Nothing faq)
        , footer = Nothing
        }


cardContact : Model -> Html ContactSubMsg
cardContact model =
    let
        loading =
            case model.contactResponse of
                Loading ->
                    True

                _ ->
                    False
    in
    Html.form
        [ onSubmit ContactClickedSubmit
        ]
        [ Elements.articleCard
            { header = text "Contact Us"
            , body =
                section []
                    [ section [ class "explanation" ]
                        [ p [] [ text "If you have any questions, feedback or feature requests, please get in touch with us." ]
                        , p [] [ text "You can contact us through the form below. A subject and body are required; a name and contact email are optional." ]
                        ]
                    , section [ class "controls", tailwind [ "mt-4" ] ]
                        [ div [ class "field" ]
                            [ Elements.label "Name" "contact-name"
                            , Elements.textInput
                                { type_ = "text"
                                , value = model.contactData.name
                                , onInput = ContactChangedName
                                , placeholder = "Name"
                                , id = "contact-name"
                                , required = False
                                }
                            ]
                        , div [ class "field" ]
                            [ Elements.label "Email" "contact-email"
                            , Elements.textInput
                                { type_ = "email"
                                , value = model.contactData.email
                                , onInput = ContactChangedEmail
                                , placeholder = "Email"
                                , id = "contact-email"
                                , required = False
                                }
                            ]
                        , div [ class "field" ]
                            [ Elements.label "Subject*" "contact-subject"
                            , Elements.textInput
                                { type_ = "text"
                                , value = model.contactData.subject
                                , onInput = ContactChangedSubject
                                , placeholder = "Subject"
                                , id = "contact-subject"
                                , required = True
                                }
                            ]
                        , div [ class "field" ]
                            [ Elements.label "Message*" "contact-body"
                            , Elements.textArea
                                { type_ = "text"
                                , value = model.contactData.body
                                , onInput = ContactChangedBody
                                , placeholder = "Message"
                                , id = "contact-body"
                                , required = True
                                }
                            ]
                        ]
                    ]
            , footer =
                Just <|
                    Elements.button
                        { text = submitContent model
                        , type_ = "submit"
                        , onClick = ContactNoOp
                        , loading = loading
                        }
            }
        ]


faq : String
faq =
    """

**AORTA** is **an open revision tool for assessments**. It is a project
developed by [CIGMAH](https://cigmah.github.io), the Coding Interest Group in
Medicine and Healthcare. AORTA is intended to provide medical students
with a free, open-access resource to help with exam revision.

This tool is a free and open source project under the GNU General
Public License v3.0. Both the [frontend](https://github.com/cigmah/aorta) and
[backend](https://github.com/cigmah/aorticroot) code are available
from our [GitHub organisation](https://github.com/cigmah). We welcome pull
requests.

This tool is on **version 0.1**. 

"""
