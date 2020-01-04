port module Types.Session exposing (..)

{-| Contains types relevant to a saved user session.

The `Session` type is the only means by which data can be shared between
pages. It primarily stores authentication credentials, though other
information that persists across pages may be stored here.

The session authentication in particular is serialized and stored as an
object in the user's local storage, so it can be deserialised when the app is
revisited to restore the previous user session.

In addition to the auth credentials, other resources (such as images) are passed as
a flag to the program so they can be bundled into the app with webpack.

-}

import Architecture.Route as Route exposing (Route)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra exposing (remove)
import RemoteData exposing (RemoteData(..), WebData)
import Types.Credentials as Credentials exposing (..)
import Types.Login as Login
import Types.Register as Register
import Types.Request as Request
import Types.Test as Test


{-| A session type, describing the data that persists across pages.
-}
type alias Session =
    { messages : Dict String String
    , authDialog : AuthDialog
    , auth : Auth
    , key : Key
    , test : Maybe Test.SessionData
    , back : Maybe Route
    , resources : Resources
    }


{-| A Resources type, indicating resources bundled with the app by webpack.

These are also passed through flags to the initial application.

-}
type alias Resources =
    { landingImage : String }


{-| Takes an authentication type and a key and creates a new default session.
-}
fillKey : Auth -> String -> Key -> Session
fillKey auth landingImage key =
    { messages = Dict.empty
    , authDialog = NoDialog
    , auth = auth
    , key = key
    , test = Nothing
    , back = Nothing
    , resources = { landingImage = landingImage }
    }


{-| Takes a key and creates a new default session.

The key can only be retrieved at runtime, so this is the closest we can get
to a truly "default" session.

-}
default : Key -> Session
default key =
    { messages = Dict.empty
    , authDialog = NoDialog
    , auth = Guest
    , key = key
    , test = Nothing
    , back = Nothing
    , resources = { landingImage = "" }
    }


{-| Checks whether a session is a guest (i.e. not authenticated).
-}
isGuest : Session -> Bool
isGuest session =
    case session.auth of
        Guest ->
            True

        User _ ->
            False


{-| Checks whether a session is a user
-}
isUser : Session -> Bool
isUser session =
    case session.auth of
        Guest ->
            False

        User _ ->
            True


{-| Create a new test for the session
-}
createTest : List Int -> Route -> Session -> ( Session, Route )
createTest questions back session =
    let
        baseTest =
            Test.init questions

        ( newTest, route ) =
            Test.unconsFuture baseTest
    in
    ( { session | test = Just newTest, back = Just back }, route )


updateTest : Test.SessionData -> Session -> Session
updateTest test session =
    { session | test = Just test }


clearTest : Session -> Session
clearTest session =
    { session | test = Nothing }


{-| Sets a back route to keep track of where the user came from.
-}
setBack : Maybe Route -> Session -> Session
setBack routeMaybe session =
    { session | back = routeMaybe }


{-| Adds a message to the session object.
-}
addMessage : ( String, String ) -> Session -> Session
addMessage ( key, value ) session =
    { session | messages = Dict.insert key value session.messages }


{-| Removes a message to the session object.
-}
clearMessage : String -> Session -> Session
clearMessage key session =
    { session | messages = Dict.remove key session.messages }


{-| A type to describe whether to show the login or register dialog.
-}
type AuthDialog
    = NoDialog
    | LoginDialog Login.PostData (WebData Credentials)
    | RegisterDialog Register.PostData (WebData Register.Response)


{-| Toggles the auth dialog on and off.
-}
toggleAuthDialog : Session -> Session
toggleAuthDialog session =
    case session.authDialog of
        -- Show the login dialog by default
        NoDialog ->
            { session | authDialog = LoginDialog Login.init NotAsked }

        _ ->
            { session | authDialog = NoDialog }


{-| Removes the auth dialog.
-}
removeAuthDialog : Session -> Session
removeAuthDialog session =
    { session | authDialog = NoDialog }


{-| Toggles the auth dialog type from login to register and vice versa.
-}
toggleAuthDialogType : Session -> Session
toggleAuthDialogType session =
    case session.authDialog of
        -- Do not alow toggling while the dialog is loading
        LoginDialog _ Loading ->
            session

        RegisterDialog _ Loading ->
            session

        LoginDialog _ _ ->
            { session | authDialog = RegisterDialog Register.init NotAsked }

        RegisterDialog _ _ ->
            { session | authDialog = LoginDialog Login.init NotAsked }

        NoDialog ->
            session


{-| Change the username for the login dialog, else do nothing.
-}
changeLoginUsername : String -> Session -> Session
changeLoginUsername string session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        LoginDialog _ Loading ->
            session

        LoginDialog data webData ->
            { session | authDialog = LoginDialog { data | username = string } webData }

        _ ->
            session


{-| Change the password for the login dialog, else do nothing.
-}
changeLoginPassword : String -> Session -> Session
changeLoginPassword string session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        LoginDialog _ Loading ->
            session

        LoginDialog data webData ->
            { session | authDialog = LoginDialog { data | password = string } webData }

        _ ->
            session


{-| Sets the login webdata.
-}
setLoginWebData : WebData Credentials -> Session -> Session
setLoginWebData webData session =
    case session.authDialog of
        LoginDialog data _ ->
            { session | authDialog = LoginDialog data webData }

        _ ->
            session


{-| Produces a login request command updated session
-}
sendLogin : (WebData Credentials -> msg) -> Session -> ( Session, Cmd msg )
sendLogin callback session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        LoginDialog _ Loading ->
            ( session, Cmd.none )

        LoginDialog data _ ->
            -- TODO: Validate the data from the frontend
            let
                newSession =
                    setLoginWebData Loading session

                request =
                    Request.postLogin
                        { data = data
                        , auth = session.auth
                        , callback = callback
                        }
            in
            ( newSession, request )

        _ ->
            ( session, Cmd.none )


{-| Handles updaing the session on login.

If login was successful, updates the session authentication and caches the
token.

-}
receiveLogin : WebData Credentials -> Session -> ( Session, Cmd msg )
receiveLogin webData session =
    let
        newSession =
            setLoginWebData webData session
    in
    case session.authDialog of
        LoginDialog _ Loading ->
            case webData of
                Success data ->
                    let
                        authSession =
                            { newSession | auth = Credentials.User data }
                    in
                    ( authSession, save authSession )

                _ ->
                    ( newSession, Cmd.none )

        _ ->
            ( session, Cmd.none )


{-| Change the username for the register dialog, else do nothing.
-}
changeRegisterUsername : String -> Session -> Session
changeRegisterUsername string session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        RegisterDialog _ Loading ->
            session

        RegisterDialog data webData ->
            { session | authDialog = RegisterDialog { data | username = string } webData }

        _ ->
            session


{-| Change the email for the register dialog, else do nothing.
-}
changeRegisterEmail : String -> Session -> Session
changeRegisterEmail string session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        RegisterDialog _ Loading ->
            session

        RegisterDialog data webData ->
            { session | authDialog = RegisterDialog { data | email = string } webData }

        _ ->
            session


{-| Sets the register webdata.
-}
setRegisterWebData : WebData Register.Response -> Session -> Session
setRegisterWebData webData session =
    case session.authDialog of
        RegisterDialog data _ ->
            { session | authDialog = RegisterDialog data webData }

        _ ->
            session


{-| Produces a register request command updated session
-}
sendRegister : (WebData Register.Response -> msg) -> Session -> ( Session, Cmd msg )
sendRegister callback session =
    case session.authDialog of
        -- Do not allow modification while the dialog is loading
        RegisterDialog _ Loading ->
            ( session, Cmd.none )

        RegisterDialog data _ ->
            -- TODO: Validate the data from the frontend
            let
                newSession =
                    setRegisterWebData Loading session

                request =
                    Request.postRegister
                        { data = data
                        , auth = session.auth
                        , callback = callback
                        }
            in
            ( newSession, request )

        _ ->
            ( session, Cmd.none )


{-| Handles updating a session on receipt of a register response.

If registration was successful, updates the session authentication and caches
the token.

-}
receiveRegister : WebData Register.Response -> Session -> ( Session, Cmd msg )
receiveRegister webData session =
    let
        newSession =
            setRegisterWebData webData session
    in
    case session.authDialog of
        RegisterDialog _ Loading ->
            case webData of
                Success data ->
                    let
                        authSession =
                            { newSession | auth = Credentials.User { username = data.username, token = data.token } }
                    in
                    ( authSession, save authSession )

                _ ->
                    ( newSession, Cmd.none )

        _ ->
            ( session, Cmd.none )


{-| A command which changes the session auth to Guest and sets the auth cache to null.
-}
logout : Session -> ( Session, Cmd msg )
logout session =
    let
        newSession =
            { session | auth = Guest }
    in
    ( newSession, save newSession )


{-| Attempts to encode a value; if it is Nothing, then encodes null.
-}
encodeMaybe : (a -> Value) -> Maybe a -> Value
encodeMaybe encoder maybeA =
    case maybeA of
        Just a ->
            encoder a

        Nothing ->
            Encode.null


{-| Encodes session authentication credentials under the key "auth".
-}
encode : Session -> Value
encode session =
    Encode.object
        [ ( "auth", Credentials.encode session.auth )
        ]


{-| Decodes credentials from the "auth" key and produces a function to fill a session.
-}
decoder : Decoder (Key -> Session)
decoder =
    Decode.map2 fillKey
        (Decode.field "auth" Credentials.decoder)
        (Decode.field "landingImage" Decode.string)


{-| A port which caches a session's authentication credentials into local storage.
-}
port cache : Value -> Cmd msg


{-| A command which saves authentication credentials in local storage.
-}
save : Session -> Cmd msg
save session =
    session
        |> encode
        |> cache
