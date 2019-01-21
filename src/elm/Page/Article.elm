module Page.Article exposing (Model, Msg, init, update, view)

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
    { articleInfo : ArticleInfo
    , content : String
    , token : Session.LoggedinToken
    , editMode : EditMode

    -- TOOD: ここでLoadking状態とかもたせればよさげ。
    }


type EditMode
    = Editor
    | Preview


type alias ArticleInfo =
    { title : String
    , id : Int
    , createdAt : String
    }


init : String -> Session.LoggedinToken -> ( Model, Cmd Msg )
init id token =
    ( Model (ArticleInfo "" 0 "") "" token Editor
    , fetchContent id token
    )



-- UPDATE


type Msg
    = ShowContent (Result Http.Error Model)
    | ChangeContent String
    | ClickedEditor
    | ClickedPreview
    | ClickedUpdate


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

        ClickedEditor ->
            ( { model | editMode = Editor }
            , Cmd.none
            )

        ClickedPreview ->
            ( { model | editMode = Preview }
            , Cmd.none
            )

        ClickedUpdate ->
            ( model
            , updateContent model
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        editModeConfig =
            case model.editMode of
                Editor ->
                    { editorDisplay = "block"
                    , editorTabclass = [ class "siimple-tabs-item", class "siimple-tabs-item--selected" ]
                    , previewDisplay = "none"
                    , previewTabclass = [ class "siimple-tabs-item" ]
                    }

                Preview ->
                    { editorDisplay = "none"
                    , editorTabclass = [ class "siimple-tabs-item" ]
                    , previewDisplay = "block"
                    , previewTabclass = [ class "siimple-tabs-item", class "siimple-tabs-item--selected" ]
                    }
    in
    div []
        [ div
            [ class "siimple-jumbotron"
            ]
            [ div [ class "siimple-jumbotron-title" ] [ text model.articleInfo.title ]
            , div [ class "siimple-jumbotron-detail" ] [ text <| "Posted at " ++ model.articleInfo.createdAt ]
            ]
        , div []
            [ span [ class "siimple-field" ] [ button [ class "siimple-btn", onClick ClickedUpdate ] [ text "update" ] ]
            , span [ class "siimple-field" ] [ button [ class "siimple-btn" ] [ text "delete" ] ]
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
            Http.get (articleUrl id) articleDecorder |> Http.toTask

        contentTask =
            Http.getString (contentUrl id) |> Http.toTask
    in
    -- I refer this redit
    -- https://www.reddit.com/r/elm/comments/91t937/is_it_possible_to_make_multiple_http_requests_in/
    Task.attempt ShowContent <|
        Task.map2 (\articleInfo content -> Model articleInfo content token Editor) articleTask contentTask


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
    Decode.map3 ArticleInfo
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "added_at" Decode.string)


updateContent : Model -> Cmd Msg
updateContent model =
    Http.send ShowContent (updateContentRequest model)


updateContentRequest : Model -> Http.Request Model
updateContentRequest model =
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ model.token)
            ]
        , url = articleUrl (String.fromInt model.articleInfo.id)
        , body = Http.stringBody "application/x-www-form-urlencoded" ("content=" ++ model.content)
        , expect = Http.expectStringResponse (\_ -> Ok model)
        , timeout = Nothing
        , withCredentials = False
        }
