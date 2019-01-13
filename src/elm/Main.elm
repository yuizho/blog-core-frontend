module Main exposing (main)

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
    { key : Nav.Key
    , page : Page
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    routeUrl url <| Model key <| ArticleListPage (ArticleList.Model [])


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
    = GoArticleList ArticleList.Msg
    | GoArticle Article.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "日常の記録"
    in
    -- decide view with Model Type
    -- refer: https://github.com/rtfeldman/elm-spa-example/blob/ad14ff6f8e50789ba59d8d2b17929f0737fc8373/src/Main.elm#L62
    case model.page of
        ArticleListPage subModel ->
            baseHtml title <| ArticleList.view subModel

        ArticlePage subModel ->
            baseHtml title <| Article.view subModel


baseHtml title content =
    { title = title
    , body = baseView title <| content
    }


baseView : String -> Html msg -> List (Html msg)
baseView title container =
    [ div
        [ class "siimple-navbar"
        , class "siimple-navbar--large"
        , class "siimple-navbar--dark"
        ]
        [ a [ class "siimple-navbar-title ", href "/" ] [ text title ]
        ]
    , div
        [ class "siimple-content"
        , class "siimple-content--large"
        ]
        [ container ]
    , div
        [ class "siimple-footer"
        , align "center"
        ]
        [ text "© 2019 Yui Ito" ]
    ]
