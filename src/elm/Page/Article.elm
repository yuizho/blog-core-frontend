module Page.Article exposing (ArticlePageMode(..), Model, Msg, OutMsg(..), init, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode
import Markdown exposing (Options, defaultOptions, toHtmlWith)
import Notification exposing (MessageType(..), Notification)
import Session
import Set exposing (..)
import Task
import Url.Builder as UrlBuilder



-- MODEL


type alias Model =
    { key : Nav.Key
    , articlePageMode : ArticlePageMode
    , articleInfo : ArticleInfo
    , content : String
    , submitAble : Bool
    , token : Session.LoggedinToken
    , editMode : EditMode
    , enteredTag : String
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
    , tags : List String
    }


init : Nav.Key -> ArticlePageMode -> Session.LoggedinToken -> ( Model, Cmd Msg )
init key articlePageMode token =
    case articlePageMode of
        Create ->
            ( Model key articlePageMode (ArticleInfo "" 0 "" []) "" False token Editor ""
            , Cmd.none
            )

        Modify id ->
            ( Model key articlePageMode (ArticleInfo "" 0 "" []) "" False token Editor ""
            , fetchContent id token
            )



-- UPDATE


type OutMsg
    = ShowMessage Notification
    | NoSignal


type Msg
    = ShowContent (Result Http.Error { articleInfo : ArticleInfo, content : String })
    | ShowContentAfterSubmit (Result Http.Error ArticleInfo)
    | AddTag String
    | ClickTagClose Int
    | ChangeTitle String
    | ChangeContent String
    | ClickedEditor
    | ClickedPreview
    | ClickedSubmit
    | ClickedDelete
    | DeleteledContent (Result Http.Error ())


update : Msg -> Model -> ( Model, Cmd Msg, OutMsg )
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
                        , submitAble = True
                      }
                    , Cmd.none
                    , NoSignal
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    , ShowMessage <| Notification Error <| getErrorMessage err
                    )

        ShowContentAfterSubmit result ->
            case result of
                Ok articleInfo ->
                    ( model
                    , Nav.pushUrl model.key <| "#/article/" ++ String.fromInt articleInfo.id
                    , ShowMessage <| Notification Success "Succeeded!!"
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    , ShowMessage <| Notification Error <| getErrorMessage err
                    )

        AddTag tag ->
            let
                articleInfo =
                    model.articleInfo

                addedTags =
                    case tag of
                        "" ->
                            model.articleInfo.tags

                        _ ->
                            (model.articleInfo.tags ++ [ tag ])
                                |> Set.fromList
                                |> Set.toList
            in
            ( { model | articleInfo = { articleInfo | tags = addedTags }, enteredTag = "" }
            , Cmd.none
            , NoSignal
            )

        ClickTagClose index ->
            let
                articleInfo =
                    model.articleInfo

                currentTags =
                    model.articleInfo.tags

                modifiedTags =
                    List.take index currentTags ++ List.drop (index + 1) currentTags
            in
            ( { model | articleInfo = { articleInfo | tags = modifiedTags } }
            , Cmd.none
            , NoSignal
            )

        ChangeTitle modifiedTitle ->
            let
                articleInfo =
                    model.articleInfo

                submitAble =
                    modifiedTitle /= ""
            in
            ( { model
                | submitAble = submitAble
                , articleInfo = { articleInfo | title = modifiedTitle }
              }
            , Cmd.none
            , NoSignal
            )

        ChangeContent modified ->
            ( { model | content = modified }
            , Cmd.none
            , NoSignal
            )

        ClickedEditor ->
            ( { model | editMode = Editor }
            , Cmd.none
            , NoSignal
            )

        ClickedPreview ->
            ( { model | editMode = Preview }
            , Cmd.none
            , NoSignal
            )

        ClickedSubmit ->
            ( model
            , sendArticle model
            , NoSignal
            )

        ClickedDelete ->
            ( model
            , deleteArticle model
            , ShowMessage <| Notification Success "Deleted!!"
            )

        DeleteledContent result ->
            case result of
                Ok _ ->
                    ( model
                    , Nav.pushUrl model.key "/"
                    , NoSignal
                    )

                Err err ->
                    ( model
                    , Cmd.none
                    , ShowMessage <| Notification Error <| getErrorMessage err
                    )


getErrorMessage : Http.Error -> String
getErrorMessage err =
    -- TODO: to be common util
    case err of
        Http.Timeout ->
            "Time out"

        Http.BadStatus resp ->
            case Decode.decodeString (Decode.field "message" Decode.string) resp.body of
                Ok message ->
                    message

                Err _ ->
                    "Unexpected Error"

        _ ->
            "Unexpected Error"



-- VIEW


view : Model -> Html Msg
view model =
    let
        submitButtonConfigs =
            if model.submitAble then
                [ class "siimple-btn--enabled", onClick ClickedSubmit ]

            else
                [ class "siimple-btn--disabled" ]

        pageModeConfig =
            case model.articlePageMode of
                Create ->
                    [ div [ class "siimple-form" ]
                        [ div
                            ([ class "siimple-btn"
                             , class "siimple-btn--dark"
                             , class "siimple--mt-4"
                             ]
                                ++ submitButtonConfigs
                            )
                            [ text "create" ]
                        ]
                    ]

                Modify string ->
                    [ div [ class "siimple-form" ]
                        [ div
                            ([ class "siimple-btn"
                             , class "siimple-btn--dark"
                             , class "siimple--mr-1"
                             , class "siimple--mt-4"
                             ]
                                ++ submitButtonConfigs
                            )
                            [ text "update" ]
                        , div
                            [ class "siimple-btn"
                            , class "siimple-btn--dark"
                            , class "siimple--mt-4"
                            , onClick ClickedDelete
                            ]
                            [ text "delete" ]
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
        [ div [ class "siimple-form" ]
            [ label [ class "siimple-label" ] [ text "Title" ]
            , input
                [ class "siimple-input"
                , class "siimple-input--fluid"
                , placeholder "title"
                , onInput ChangeTitle
                , value model.articleInfo.title
                ]
                [ text model.articleInfo.title ]
            ]
        , div [ class "siimple-form" ]
            [ label [ class "siimple-label" ] [ text "Tag" ]
            , input
                [ class "siimple-input"
                , class "siimple-input--fluid"
                , placeholder "additional tag name"
                , onBlurWithTargetValue AddTag
                , onEnter AddTag
                , value model.enteredTag
                ]
                [ text model.enteredTag ]
            , Keyed.node "div"
                []
                (List.indexedMap tagElements model.articleInfo.tags)
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
            , div [] pageModeConfig
            ]
        ]


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue tagger =
    on "blur" (Decode.map tagger targetValue)


onEnter : (String -> msg) -> Attribute msg
onEnter tagger =
    let
        isEnter code =
            -- 13 is enter key code
            if code == 13 then
                Decode.map tagger targetValue

            else
                Decode.fail "not ENTER"
    in
    on "keydown" (Decode.andThen isEnter keyCode)


tagElements : Int -> String -> ( String, Html Msg )
tagElements index tag =
    ( String.fromInt index
    , span
        [ class "siimple-tag"
        , class "siimple-tag--primary"
        , class "siimple-tag--rounded"
        , class "siimple--mt-2"
        , class "siimple--mr-1"
        ]
        [ text tag
        , div
            [ class "siimple-close"
            , onClick (ClickTagClose index)
            ]
            []
        ]
    )


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
    UrlBuilder.absolute [ "api", "articles", id, "content" ] []


articleUrl : String
articleUrl =
    UrlBuilder.absolute [ "api", "articles" ] []


createdArticleUrl : String -> String
createdArticleUrl id =
    UrlBuilder.absolute [ "api", "articles", id ] []


articleDecorder : Decode.Decoder ArticleInfo
articleDecorder =
    Decode.map4 ArticleInfo
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "added_at" Decode.string)
        (Decode.field "tag_names" (Decode.list Decode.string))


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

        tags =
            String.join "," model.articleInfo.tags
    in
    Http.request
        { method = method
        , headers =
            [ Http.header "X-Requested-With" "XMLHttpRequest"
            , Http.header "Authorization" ("token " ++ model.token)
            ]
        , url = url
        , body =
            Http.stringBody "application/x-www-form-urlencoded"
                ("content="
                    ++ model.content
                    ++ "&title="
                    ++ model.articleInfo.title
                    ++ "&tags="
                    ++ tags
                )
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
