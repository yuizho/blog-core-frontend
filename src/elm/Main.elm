port module Main exposing (Credential, LoginData, Model, Msg(..), Page(..), Session(..), baseHtml, baseView, init, loginDecorder, loginUrl, main, portGetLocalStorage, portResLocalStorage, portSetLocalStorage, postLogin, route, routeParser, routeUrl, stepArticle, stepArticleList, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Page.Article as Article
import Page.ArticleList as ArticleList
import Task
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
    , page : Page
    }


type alias Credential =
    { username : String
    , password : String
    }


type Session
    = Loggedin String
    | Guest (Maybe Credential)


type alias LoginData =
    { token : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model url key (Guest Nothing) <| ArticleListPage (ArticleList.Model [])
    , portGetLocalStorage "loggein_token"
    )


type Page
    = ArticleListPage ArticleList.Model
    | ArticlePage Article.Model


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
        , route (s "article" </> string)
            (\id -> stepArticle model (Article.init id))
        ]


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    map handler parser


stepArticleList : Model -> ( ArticleList.Model, Cmd ArticleList.Msg ) -> ( Model, Cmd Msg )
stepArticleList model ( articlelist, cmds ) =
    ( { model | page = ArticleListPage articlelist }
    , Cmd.map GoArticleList cmds
    )


stepArticle : Model -> ( Article.Model, Cmd Article.Msg ) -> ( Model, Cmd Msg )
stepArticle model ( article, cmds ) =
    ( { model | page = ArticlePage article }
    , Cmd.map GoArticle cmds
    )



-- UPDATE


type Msg
    = ReceiveSessionStatus (Maybe String)
    | ChangeUserName String
    | ChangePassword String
    | Login
    | LoggedinSession (Result Http.Error LoginData)
    | Logout
    | LoggedoutSession (Result Http.Error ())
    | GoArticleList ArticleList.Msg
    | GoArticle Article.Msg
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
                    ( model, postLogout token )

                Guest _ ->
                    ( model, Cmd.none )

        LoggedoutSession result ->
            case result of
                Ok login ->
                    ( { model | session = Guest Nothing }
                    , portRemoveLocalStorage "loggein_token"
                    )

                Err _ ->
                    ( model, Cmd.none )

        GoArticleList subMsg ->
            case model.page of
                ArticleListPage article ->
                    stepArticleList model (ArticleList.update subMsg article)

                _ ->
                    ( model, Cmd.none )

        GoArticle subMsg ->
            case model.page of
                ArticlePage article ->
                    stepArticle model (Article.update subMsg article)

                _ ->
                    ( model, Cmd.none )

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



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "日常の記録"
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
                    baseHtml model title (Html.map (\subMsg -> GoArticle subMsg) <| Article.view subModel)


baseHtml model title content =
    { title = title
    , body = baseView model title <| content
    }


baseView : Model -> String -> Html Msg -> List (Html Msg)
baseView model title container =
    [ viewNavBar model title
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
                        [ div [ class "siimple-navbar-item", onClick Logout ] [ text "Logout" ]
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
