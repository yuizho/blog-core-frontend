module Page.Settings exposing (Model, Msg, OutMsg(..), init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Notification exposing (MessageType(..), Notification)
import Session
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { key : Nav.Key
    , token : Session.LoggedinToken
    , currentPassword : Maybe String
    , newPassword1 : Maybe String
    , newPassword2 : Maybe String
    , sendAblePassword : Bool
    }


init : Nav.Key -> Session.LoggedinToken -> ( Model, Cmd Msg )
init key token =
    ( Model key token Nothing Nothing Nothing False, Cmd.none )



-- UPDATE


type OutMsg
    = ShowMessage Notification
    | RemoveMessage
    | NoSignal


type Msg
    = UpdateOldPassword String
    | UpdatedNewPassword1 String
    | UpdatedNewPassword2 String
    | ChangePassword
    | ResultChangePassword (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update msg model =
    case msg of
        UpdateOldPassword oldPass ->
            ( { model | currentPassword = Just oldPass }
            , Cmd.none
            , NoSignal
            )

        UpdatedNewPassword1 newPass1 ->
            let
                ( outMsg, isEnable ) =
                    case model.newPassword2 of
                        Just newPass2 ->
                            if newPass1 == "" then
                                ( RemoveMessage, False )

                            else if newPass1 == newPass2 then
                                ( RemoveMessage, True )

                            else
                                ( ShowMessage (Notification Error "The entered New Passwords are not same."), False )

                        Nothing ->
                            ( NoSignal, False )
            in
            ( { model | newPassword1 = Just newPass1, sendAblePassword = isEnable }
            , Cmd.none
            , outMsg
            )

        UpdatedNewPassword2 newPass2 ->
            let
                ( outMsg, isEnable ) =
                    case model.newPassword1 of
                        Just newPass1 ->
                            if newPass2 == "" then
                                ( RemoveMessage, False )

                            else if newPass1 == newPass2 then
                                ( RemoveMessage, True )

                            else
                                ( ShowMessage (Notification Error "The entered New Passwords are not same."), False )

                        Nothing ->
                            ( NoSignal, False )
            in
            ( { model | newPassword2 = Just newPass2, sendAblePassword = isEnable }
            , Cmd.none
            , outMsg
            )

        ChangePassword ->
            ( model
            , changePassword model
            , NoSignal
            )

        ResultChangePassword result ->
            case result of
                Ok _ ->
                    ( model
                    , Cmd.none
                    , ShowMessage <| Notification Success "Password was changed!!"
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    , ShowMessage <| Notification Error "Current Password is wrong."
                    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        passwordButtonConfig =
            if model.sendAblePassword then
                [ class "siimple-btn", class "siimple-btn--dark", onClick ChangePassword ]

            else
                [ class "siimple-btn", class "siimple-btn--dark", class "siimple-btn--disabled" ]
    in
    div []
        [ div
            [ class "siimple-card" ]
            [ div [ class "siimple-card-header" ] [ text "Password" ]
            , div [ class "siimple-card-body" ]
                [ div [ class "siimple-form" ]
                    [ label [ class "siimple-label" ] [ text "Old Password" ]
                    , input
                        [ class "siimple-input"
                        , class "siimple-input--fluid"
                        , placeholder "old password"
                        , style "margin-bottom" "10px"
                        , type_ "password"
                        , onInput UpdateOldPassword
                        ]
                        []
                    , label [ class "siimple-label" ] [ text "New Password" ]
                    , input
                        [ class "siimple-input"
                        , class "siimple-input--fluid"
                        , placeholder "new password"
                        , style "margin-bottom" "10px"
                        , type_ "password"
                        , onInput UpdatedNewPassword1
                        ]
                        []
                    , label [ class "siimple-label" ] [ text "Re Enter New Password" ]
                    , input
                        [ class "siimple-input"
                        , class "siimple-input--fluid"
                        , placeholder "new password"
                        , style "margin-bottom" "20px"
                        , type_ "password"
                        , onInput UpdatedNewPassword2
                        ]
                        []
                    , div
                        passwordButtonConfig
                        [ text "Update" ]
                    , br [] []
                    ]
                ]
            ]
        ]



-- HTTP


passwordUrl : String
passwordUrl =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "user", "password" ]
        []


changePassword : Model -> Cmd Msg
changePassword model =
    Http.send ResultChangePassword (changePasswordRequest model)


changePasswordRequest : Model -> Http.Request ()
changePasswordRequest model =
    let
        currentPassword =
            Maybe.withDefault "" model.currentPassword

        newPassword =
            Maybe.withDefault "" model.newPassword1
    in
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ model.token)
            ]
        , url = passwordUrl
        , body = Http.stringBody "application/x-www-form-urlencoded" ("current_password=" ++ currentPassword ++ "&new_password=" ++ newPassword)
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
