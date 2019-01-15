module Page.Article exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Task
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { articleInfo : ArticleInfo
    , content : String

    -- TOOD: ここでLoadking状態とかもたせればよさげ。
    }


type alias ArticleInfo =
    { title : String
    , createdAt : String
    }


init : String -> ( Model, Cmd Msg )
init id =
    ( Model (ArticleInfo "" "") ""
    , fetchContent id
    )



-- UPDATE


type Msg
    = ShowContent (Result Http.Error Model)
    | ChangeContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowContent result ->
            case result of
                Ok content ->
                    -- TODO: when came here directly, some loading image shold be shown
                    ( content
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        ChangeContent modified ->
            ( { model | content = modified }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ class "siimple-jumbotron"
            ]
            [ div [ class "siimple-jumbotron-title" ] [ text model.articleInfo.title ]
            , div [ class "siimple-jumbotron-detail" ] [ text <| "Posted at " ++ model.articleInfo.createdAt ]
            ]
        , div
            [ class "siimple-rule"
            , class "siimple--color-dark"
            ]
            []
        , div []
            [ div []
                [ textarea
                    [ class "siimple-textarea"
                    , class "siimple-textarea--fluid"
                    , rows 10
                    , onInput ChangeContent
                    ]
                    [ text model.content ]
                ]
            , div [] [ toHtmlWith options [] model.content ]
            ]
        ]


options : Options
options =
    { defaultOptions | sanitize = True }



-- HTTP


fetchContent : String -> Cmd Msg
fetchContent id =
    let
        articleTask =
            Http.get (articleUrl id) articleDecorder |> Http.toTask

        contentTask =
            Http.getString (contentUrl id) |> Http.toTask
    in
    -- I refer this redit
    -- https://www.reddit.com/r/elm/comments/91t937/is_it_possible_to_make_multiple_http_requests_in/
    Task.attempt ShowContent <|
        Task.map2 (\articleInfo content -> Model articleInfo content) articleTask contentTask


contentUrl : String -> String
contentUrl id =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles", id, "content" ]
        []


articleUrl : String -> String
articleUrl id =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles", id ]
        []


articleDecorder : Decode.Decoder ArticleInfo
articleDecorder =
    Decode.map2 ArticleInfo
        (Decode.field "title" Decode.string)
        (Decode.field "added_at" Decode.string)
