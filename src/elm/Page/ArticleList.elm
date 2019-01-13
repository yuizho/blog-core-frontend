module Page.ArticleList exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { articles : List Article

    -- TOOD: ここでLoadking状態とかもたせればよさげ。
    }


type alias Article =
    { title : String
    , id : Int
    , createdAt : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model []
    , fetchArticles
    )



-- UPDATE


type Msg
    = ShowArticles (Result Http.Error (List Article))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowArticles result ->
            case result of
                Ok newArticle ->
                    ( { model | articles = newArticle }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )



-- VIEW


view : Model -> Html msg
view model =
    div [ class "siimple-grid-row" ] (List.map viewArticle model.articles)


viewArticle : Article -> Html msg
viewArticle article =
    div
        [ class "siimple-grid-col"
        , class "siimple-grid-col--9"
        ]
        [ a
            [ class "siimple-link"
            , class "siimple--color-dark"
            , href ("#/article/" ++ String.fromInt article.id)
            ]
            [ text article.title ]
        , div [ class "siimple-small" ] [ text article.createdAt ]
        ]



-- HTTP


fetchArticles : Cmd Msg
fetchArticles =
    Http.send ShowArticles (Http.get articlesUrl articlesDecorder)


articlesUrl : String
articlesUrl =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles" ]
        []


articlesDecorder : Decode.Decoder (List Article)
articlesDecorder =
    Decode.list
        (Decode.map3 Article
            (Decode.field "title" Decode.string)
            (Decode.field "id" Decode.int)
            (Decode.field "added_at" Decode.string)
        )
