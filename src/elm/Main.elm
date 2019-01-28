port module Main exposing (Msg(..), portGetLocalStorage, portResLocalStorage, portSetLocalStorage, postLogin)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Notification exposing (..)
import Page.Article as Article
import Page.ArticleList as ArticleList
import Page.Settings as Settings
import Process
import Session exposing (Credential, Session(..))
import Task
import Time
import Tuple
import Url
import Url.Builder as UrlBuilder
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)



-- PORT


port portSetLocalStorage : ( String, String ) -> Cmd msg


port portGetLocalStorage : String -> Cmd msg


port portResLocalStorage : (Maybe String -> msg) -> Sub msg


port portRemoveLocalStorage : String -> Cmd msg



-- MAIN


main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type alias Model =
    { url : Url.Url
    , key : Nav.Key
    , session : Session
    , notification : Maybe Notification
    , page : Page
    }


type alias LoginData =
    { token : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model url key (Guest Nothing) Nothing <| ArticleListPage (ArticleList.Model [])
    , portGetLocalStorage "loggein_token"
    )


type Page
    = ArticleListPage ArticleList.Model
    | ArticlePage Article.Model
    | SettingsPage Settings.Model


routeUrl : Url.Url -> Model -> ( Model, Cmd Msg )
routeUrl url model =
    let
        -- The RealWorld spec treats the fragment like a path.
        -- This makes it *literally* the path, so we can proceed
        -- with parsing as if it had been a normal path all along.
        -- I refered This
        -- https://github.com/rtfeldman/elm-spa-example/blob/b5064c6ef0fde3395a7299f238acf68f93e71d03/src/Route.elm#L59
        parsed =
            { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
                |> parse (routeParser model)
    in
    case parsed of
        Just result ->
            result

        Nothing ->
            ( { model | page = ArticleListPage (ArticleList.Model []) }
            , Cmd.none
            )


routeParser : Model -> Parser (( Model, Cmd Msg ) -> a) a
routeParser model =
    oneOf
        [ route top
            (stepArticleList model ArticleList.init)
        , route (s "article") <|
            case model.session of
                Session.Loggedin token ->
                    stepArticle model (Article.init model.key Article.Create token)

                Session.Guest _ ->
                    stepArticleList model ArticleList.init
        , route (s "article" </> string) <|
            case model.session of
                Session.Loggedin token ->
                    \id -> stepArticle model (Article.init model.key (Article.Modify id) token)

                Session.Guest _ ->
                    \_ -> stepArticleList model ArticleList.init
        , route (s "settings") <|
            case model.session of
                Session.Loggedin token ->
                    stepSettings model (Settings.init model.key token)

                Session.Guest _ ->
                    stepArticleList model ArticleList.init
        ]


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    map handler parser


stepArticleList : Model -> ( ArticleList.Model, Cmd ArticleList.Msg ) -> ( Model, Cmd Msg )
stepArticleList model ( articlelist, cmds ) =
    ( { model | page = ArticleListPage articlelist }
    , Cmd.map ArticleListUpdate cmds
    )


stepArticle : Model -> ( Article.Model, Cmd Article.Msg ) -> ( Model, Cmd Msg )
stepArticle model ( article, cmds ) =
    ( { model | page = ArticlePage article }
    , Cmd.map ArticleUpdate cmds
    )


stepSettings : Model -> ( Settings.Model, Cmd Settings.Msg ) -> ( Model, Cmd Msg )
stepSettings model ( settings, cmds ) =
    ( { model | page = SettingsPage settings }
    , Cmd.map SettingsUpdate cmds
    )


processArticleSignal : Article.OutMsg -> Model -> ( Model, Cmd Msg )
processArticleSignal signal model =
    case signal of
        Article.ShowMessage notification ->
            ( { model | notification = Just notification }, closeMessageAsync )

        Article.NoSignal ->
            ( model, Cmd.none )


processSettingsSignal : Settings.OutMsg -> Model -> ( Model, Cmd Msg )
processSettingsSignal signal model =
    case signal of
        Settings.ShowMessage notification ->
            ( { model | notification = Just notification }, closeMessageAsync )

        Settings.RemoveMessage ->
            ( { model | notification = Nothing }, Cmd.none )

        Settings.NoSignal ->
            ( model, Cmd.none )



-- UPDATE


type Msg
    = ReceiveSessionStatus (Maybe String)
    | ChangeUserName String
    | ChangePassword String
    | Login
    | LoggedinSession (Result Http.Error LoginData)
    | Logout
    | LoggedoutSession (Result Http.Error ())
    | ArticleListUpdate ArticleList.Msg
    | ArticleUpdate Article.Msg
    | SettingsUpdate Settings.Msg
    | CloseMessage
    | CloseMessageAsync ()
    | ShowMessage Notification
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceiveSessionStatus value ->
            case value of
                Just token ->
                    routeUrl model.url <| { model | session = Loggedin token }

                Nothing ->
                    routeUrl model.url <| { model | session = Guest Nothing }

        ChangeUserName value ->
            case model.session of
                Guest maybeCred ->
                    case maybeCred of
                        Just cred ->
                            ( { model | session = Guest <| Just { cred | username = value } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | session = Guest <| Just (Credential value "") }
                            , Cmd.none
                            )

                Loggedin _ ->
                    ( model, Cmd.none )

        ChangePassword value ->
            case model.session of
                Guest maybeCred ->
                    case maybeCred of
                        Just cred ->
                            ( { model | session = Guest <| Just { cred | password = value } }
                            , Cmd.none
                            )

                        Nothing ->
                            ( { model | session = Guest <| Just (Credential "" value) }
                            , Cmd.none
                            )

                Loggedin _ ->
                    ( model, Cmd.none )

        Login ->
            case model.session of
                Guest maybeCred ->
                    case maybeCred of
                        Just cred ->
                            ( model
                            , postLogin cred
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Loggedin _ ->
                    ( model, Cmd.none )

        LoggedinSession result ->
            case result of
                Ok login ->
                    ( { model | session = Loggedin login.token }
                    , portSetLocalStorage ( "loggein_token", login.token )
                    )

                Err _ ->
                    -- TOOD: Error handling
                    ( model
                    , Cmd.none
                    )

        Logout ->
            case model.session of
                Loggedin token ->
                    -- TODO: postLogoutとportRemoveLocalStrageはTaskで同時にやったほうがよさげ
                    ( model, Cmd.batch [ portRemoveLocalStorage "loggein_token", postLogout token ] )

                Guest _ ->
                    ( model, Cmd.none )

        LoggedoutSession result ->
            case result of
                Ok login ->
                    ( { model | session = Guest Nothing }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        ArticleListUpdate subMsg ->
            case model.page of
                ArticleListPage article ->
                    stepArticleList model (ArticleList.update subMsg article)

                _ ->
                    ( model, Cmd.none )

        ArticleUpdate subMsg ->
            case model.page of
                ArticlePage articleModel ->
                    let
                        ( newArticleModel, childCommands, signalForParent ) =
                            Article.update subMsg articleModel

                        ( newModel, cmdsFromSignal ) =
                            processArticleSignal signalForParent { model | page = ArticlePage newArticleModel }
                    in
                    ( newModel
                    , Cmd.batch
                        [ Cmd.map ArticleUpdate childCommands
                        , cmdsFromSignal
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        SettingsUpdate subMsg ->
            case model.page of
                SettingsPage articleModel ->
                    let
                        ( newSubModel, childCommands, signalForParent ) =
                            Settings.update subMsg articleModel

                        ( newModel, cmdsFromSignal ) =
                            processSettingsSignal signalForParent { model | page = SettingsPage newSubModel }
                    in
                    ( newModel
                    , Cmd.batch
                        [ Cmd.map SettingsUpdate childCommands
                        , cmdsFromSignal
                        ]
                    )

                _ ->
                    ( model, Cmd.none )

        ShowMessage notification ->
            ( { model | notification = Just notification }, closeMessageAsync )

        CloseMessage ->
            ( { model | notification = Nothing }
            , Cmd.none
            )

        CloseMessageAsync _ ->
            ( { model | notification = Nothing }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            routeUrl url model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    portResLocalStorage ReceiveSessionStatus


sleepTask : Task.Task Never ()
sleepTask =
    Process.sleep 3000 |> Task.map (\_ -> ())


closeMessageAsync : Cmd Msg
closeMessageAsync =
    Task.perform CloseMessageAsync sleepTask



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "Blog Manager"
    in
    -- decide view with Model Type
    -- refer: https://github.com/rtfeldman/elm-spa-example/blob/ad14ff6f8e50789ba59d8d2b17929f0737fc8373/src/Main.elm#L62
    case model.session of
        Guest _ ->
            baseHtml model title <|
                div []
                    [ div [ class "siimple-field" ]
                        [ div [] [ text "user name" ]
                        , input [ class "siimple-input", onInput ChangeUserName ] []
                        ]
                    , div [ class "siimple-field" ]
                        [ div [] [ text "password" ]
                        , input [ class "siimple-input", type_ "password", onInput ChangePassword ] []
                        ]
                    , div [ class "siimple-field" ] [ button [ class "siimple-btn", onClick Login ] [ text "login" ] ]
                    ]

        Loggedin token ->
            case model.page of
                ArticleListPage subModel ->
                    baseHtml model title <| ArticleList.view subModel

                ArticlePage subModel ->
                    baseHtml model title (Html.map (\subMsg -> ArticleUpdate subMsg) <| Article.view subModel)

                SettingsPage subModel ->
                    baseHtml model title (Html.map (\subMsg -> SettingsUpdate subMsg) <| Settings.view subModel)


baseHtml model title content =
    { title = title
    , body = baseView model title <| content
    }


baseView : Model -> String -> Html Msg -> List (Html Msg)
baseView model title container =
    [ viewNavBar model title
    , viewNotifyIfNeeded model.notification
    , div
        [ class "siimple-content"
        , class "siimple-content--large"
        ]
        [ container ]
    , div
        [ class "siimple-footer"
        , align "center"
        ]
        []
    ]


viewNavBar model title =
    let
        titleDom =
            a [ class "siimple-navbar-title ", href "/" ] [ text title ]

        navContents =
            case model.session of
                Loggedin _ ->
                    [ titleDom
                    , div [ class "siimple--float-right" ]
                        [ a [ class "siimple-navbar-item", href "#/article" ] [ text "Create" ]
                        , a [ class "siimple-navbar-item", href "#/settings" ] [ text "Settings" ]
                        , div [ class "siimple-navbar-item", onClick Logout ] [ text "Logout" ]
                        ]
                    ]

                Guest _ ->
                    [ titleDom ]
    in
    div
        [ class "siimple-navbar"
        , class "siimple-navbar--large"
        , class "siimple-navbar--dark"
        ]
        navContents


viewNotifyIfNeeded : Maybe Notification -> Html Msg
viewNotifyIfNeeded notification =
    notification
        |> Maybe.map (\n -> viewNotification n)
        |> Maybe.withDefault (div [] [])


viewNotification : Notification -> Html Msg
viewNotification notification =
    let
        messageTypeClass =
            case notification.messageType of
                Info ->
                    "siimple-alert--primary"

                Success ->
                    "siimple-alert--success"

                Warn ->
                    "siimple-alert--warning"

                Error ->
                    "siimple-alert--error"
    in
    div
        [ class "siimple-alert"
        , class messageTypeClass
        , style "height" "40px"
        , style "position" "fixed"
        , style "top" "0"
        , style "z-index" "999"
        , onClick CloseMessage
        ]
        [ div [ class "siimple-alert-close", onClick CloseMessage ] []
        , text notification.message
        ]



-- HTPP


postLogin : Credential -> Cmd Msg
postLogin cred =
    let
        body =
            "id=" ++ cred.username ++ "&password=" ++ cred.password
    in
    Http.send LoggedinSession (loginRequest loginUrl body)


loginRequest : String -> String -> Http.Request LoginData
loginRequest url body =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest" ]
        , url = url
        , body = Http.stringBody "application/x-www-form-urlencoded" body
        , expect = Http.expectJson loginDecorder
        , timeout = Nothing
        , withCredentials = False
        }


loginUrl : String
loginUrl =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "user", "login" ]
        []


loginDecorder : Decode.Decoder LoginData
loginDecorder =
    Decode.map LoginData
        (Decode.field "token" Decode.string)


postLogout : String -> Cmd Msg
postLogout token =
    Http.send LoggedoutSession (logoutRequest logoutUrl token)


logoutRequest : String -> String -> Http.Request ()
logoutRequest url token =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ token)
            ]
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


logoutUrl : String
logoutUrl =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "user", "logout" ]
        []
