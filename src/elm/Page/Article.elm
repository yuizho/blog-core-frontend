module Page.Article exposing (ArticlePageMode(..), Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Notification exposing (MessageType(..), Notification)
import Session
import Task
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { key : Nav.Key
    , articlePageMode : ArticlePageMode
    , articleInfo : ArticleInfo
    , content : String
    , token : Session.LoggedinToken
    , editMode : EditMode
    , notification : Maybe Notification

    -- TOOD: ここでLoadding状態とかもたせればよさげ。おそらく、Modelのtypeも分けるとなおいいかな。
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


init : Nav.Key -> ArticlePageMode -> Session.LoggedinToken -> ( Model, Cmd Msg )
init key articlePageMode token =
    case articlePageMode of
        Create ->
            ( Model key articlePageMode (ArticleInfo "" 0 "") "" token Editor Nothing
            , Cmd.none
            )

        Modify id ->
            ( Model key articlePageMode (ArticleInfo "" 0 "") "" token Editor Nothing
            , fetchContent id token
            )



-- UPDATE


type Msg
    = ShowContent (Result Http.Error { articleInfo : ArticleInfo, content : String })
    | ShowContentAfterSubmit (Result Http.Error ArticleInfo)
    | ChangeTitle String
    | ChangeContent String
    | CloseMessage
    | ClickedEditor
    | ClickedPreview
    | ClickedSubmit
    | ClickedDelete
    | DeleteledContent (Result Http.Error ())


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
                    ( { model | notification = Just <| Notification Error "Unexpected Error was occurred......" }
                    , Cmd.none
                    )

        ShowContentAfterSubmit result ->
            case result of
                Ok articleInfo ->
                    -- TODO: when came here directly, some loading image shold be shown
                    ( { model
                        | articleInfo = articleInfo
                        , articlePageMode = Modify (String.fromInt <| articleInfo.id)
                        , notification = Just <| Notification Success "Succeeded!!"
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | notification = Just <| Notification Error "Unexpected Error was occurred......" }
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

        CloseMessage ->
            ( { model | notification = Nothing }
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

        ClickedDelete ->
            ( model
            , deleteArticle model
            )

        DeleteledContent result ->
            case result of
                Ok _ ->
                    ( model, Nav.pushUrl model.key "/" )

                Err _ ->
                    ( { model | notification = Just <| Notification Error "Unexpected Error was occurred......" }
                    , Cmd.none
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
                        , button [ class "siimple-btn", onClick ClickedDelete ] [ text "delete" ]
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
        [ viewNotifyIfNeeded model.notification
        , div [] pageModeConfig
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
    div [ class "siimple-alert", class messageTypeClass ]
        [ div [ class "siimple-alert-close", onClick CloseMessage ] []
        , text notification.message
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
    Http.send ShowContentAfterSubmit (sendArticleRequest model)


sendArticleRequest : Model -> Http.Request ArticleInfo
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
        , expect = Http.expectJson articleDecorder
        , timeout = Nothing
        , withCredentials = False
        }


deleteArticle : Model -> Cmd Msg
deleteArticle model =
    Http.send DeleteledContent (deleteArticleRequest model)


deleteArticleRequest : Model -> Http.Request ()
deleteArticleRequest model =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ model.token)
            ]
        , url = createdArticleUrl (String.fromInt model.articleInfo.id)
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }
