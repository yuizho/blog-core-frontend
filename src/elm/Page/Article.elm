module Page.Article exposing (ArticlePageMode(..), Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Session
import Task
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { articlePageMode : ArticlePageMode
    , articleInfo : ArticleInfo
    , content : String
    , token : Session.LoggedinToken
    , editMode : EditMode

    -- TOOD: ここでLoadking状態とかもたせればよさげ。
    }


type ArticlePageMode
    = Create
    | Modify String


type EditMode
    = Editor
    | Preview


type alias ArticleInfo =
    { title : String
    , id : Int
    , createdAt : String
    }


init : ArticlePageMode -> Session.LoggedinToken -> ( Model, Cmd Msg )
init articlePageMode token =
    case articlePageMode of
        Create ->
            ( Model articlePageMode (ArticleInfo "" 0 "") "" token Editor
            , Cmd.none
            )

        Modify id ->
            ( Model articlePageMode (ArticleInfo "" 0 "") "" token Editor
            , fetchContent id token
            )



-- UPDATE


type Msg
    = ShowContent (Result Http.Error { articleInfo : ArticleInfo, content : String })
    | ChangeTitle String
    | ChangeContent String
    | ClickedEditor
    | ClickedPreview
    | ClickedSubmit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowContent result ->
            case result of
                Ok data ->
                    -- TODO: when came here directly, some loading image shold be shown
                    ( { model
                        | articleInfo = data.articleInfo
                        , content = data.content
                        , articlePageMode = Modify (String.fromInt <| data.articleInfo.id)
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( model
                    , Cmd.none
                    )

        ChangeTitle modifiedTitle ->
            let
                articleInfo =
                    model.articleInfo
            in
            ( { model
                | articleInfo = { articleInfo | title = modifiedTitle }
              }
            , Cmd.none
            )

        ChangeContent modified ->
            ( { model | content = modified }
            , Cmd.none
            )

        ClickedEditor ->
            ( { model | editMode = Editor }
            , Cmd.none
            )

        ClickedPreview ->
            ( { model | editMode = Preview }
            , Cmd.none
            )

        ClickedSubmit ->
            ( model
            , sendArticle model
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        pageModeConfig =
            case model.articlePageMode of
                Create ->
                    [ span [ class "siimple-field" ] [ button [ class "siimple-btn", onClick ClickedSubmit ] [ text "create" ] ]
                    ]

                Modify string ->
                    [ span [ class "siimple-field" ]
                        [ button [ class "siimple-btn", onClick ClickedSubmit ] [ text "update" ]
                        , button [ class "siimple-btn" ] [ text "delete" ]
                        ]
                    ]

        editModeConfig =
            case model.editMode of
                Editor ->
                    { editorDisplay = "block"
                    , editorTabclass = [ class "siimple-tabs-item", class "siimple-tabs-item--selected" ]
                    , previewDisplay = "none"
                    , previewTabclass =
                        [ class "siimple-tabs-item" ]
                    }

                Preview ->
                    { editorDisplay = "none"
                    , editorTabclass = [ class "siimple-tabs-item" ]
                    , previewDisplay = "block"
                    , previewTabclass = [ class "siimple-tabs-item", class "siimple-tabs-item--selected" ]
                    }
    in
    div []
        [ div [] pageModeConfig
        , div []
            [ input
                [ class "siimple-input"
                , class "siimple-input--fluid"
                , placeholder "title"
                , onInput ChangeTitle
                , value model.articleInfo.title
                ]
                [ text model.articleInfo.title ]
            ]
        , div []
            [ div
                [ class "siimple-tabs"
                , class "siimple-tabs--boxed"
                ]
                [ div (onClick ClickedEditor :: editModeConfig.editorTabclass) [ text "Editor" ]
                , div (onClick ClickedPreview :: editModeConfig.previewTabclass) [ text "Preview" ]
                ]
            , div []
                [ textarea
                    [ class "siimple-textarea"
                    , class "siimple-textarea--fluid"
                    , rows 25
                    , onInput ChangeContent
                    , style "display" editModeConfig.editorDisplay
                    ]
                    [ text model.content ]
                ]
            , div [ style "display" editModeConfig.previewDisplay ] [ toHtmlWith options [] model.content ]
            ]
        ]


options : Options
options =
    { defaultOptions | sanitize = True }



-- HTTP


fetchContent : String -> Session.LoggedinToken -> Cmd Msg
fetchContent id token =
    let
        articleTask =
            Http.get (createdArticleUrl id) articleDecorder |> Http.toTask

        contentTask =
            Http.getString (contentUrl id) |> Http.toTask
    in
    -- I refer this redit
    -- https://www.reddit.com/r/elm/comments/91t937/is_it_possible_to_make_multiple_http_requests_in/
    Task.attempt ShowContent <|
        Task.map2 (\articleInfo content -> { articleInfo = articleInfo, content = content }) articleTask contentTask


contentUrl : String -> String
contentUrl id =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles", id, "content" ]
        []


articleUrl : String
articleUrl =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles" ]
        []


createdArticleUrl : String -> String
createdArticleUrl id =
    UrlBuilder.crossOrigin "http://localhost:8080"
        [ "api", "articles", id ]
        []


articleDecorder : Decode.Decoder ArticleInfo
articleDecorder =
    Decode.map3 ArticleInfo
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "added_at" Decode.string)


sendArticle : Model -> Cmd Msg
sendArticle model =
    Http.send ShowContent (sendArticleRequest model)


sendArticleRequest : Model -> Http.Request { articleInfo : ArticleInfo, content : String }
sendArticleRequest model =
    let
        ( method, url ) =
            case model.articlePageMode of
                Create ->
                    ( "POST", articleUrl )

                Modify id ->
                    ( "PUT", createdArticleUrl (String.fromInt model.articleInfo.id) )
    in
    Http.request
        { method = method
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ model.token)
            ]
        , url = url
        , body = Http.stringBody "application/x-www-form-urlencoded" ("content=" ++ model.content ++ "&title=" ++ model.articleInfo.title)
        , expect = Http.expectStringResponse (\_ -> Ok { articleInfo = model.articleInfo, content = model.content })
        , timeout = Nothing
        , withCredentials = False
        }
