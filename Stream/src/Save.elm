module Save exposing (Msg(..), copyJavaCommentsButton, debug, downloadButton, update, uploadButton, view)

{--

  Converts a model as described in Tree.Core to and from JSON
    Model.highlightedBox is reset to Nothing, other values are passed on as is

  Legend: Update, Subscriptions, View, Encoding, Decoding

--}

import Base64 exposing (decode)
import Color exposing (white)
import Css exposing (..)
import Debug exposing (log)
import File exposing (File)
import File.Download as Download exposing (string)
import File.Select as Select
import Html.Styled exposing (Html, a, br, button, div, input, li, text, ul)
import Html.Styled.Attributes exposing (autofocus, class, css, href, id, multiple, placeholder, style, type_)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Ports exposing (downloadToast, checkpoint)
import Task exposing (perform)
import Tree.Core as Tree exposing (..)
import Tree.Draw exposing (treeWithConditions)
import Tree.State as State exposing (..)


type Msg
    = GenerateJavaComments
    | DownloadJson
    | UploadRequested
    | UploadLoaded File
    | UploadRead String
    | Checkpoint


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Checkpoint ->
            ( model
            , checkpoint <| State.toJson model
            )

        GenerateJavaComments ->
            ( model
            , downloadToast <| State.toJson model
            )

        DownloadJson ->
            ( model, Download.string (model.flowchartName ++ ".flow") "application/flow" (toJson model) )

        UploadRequested ->
            ( model, Select.file [ "application/flow" ] UploadLoaded )

        UploadLoaded file ->
            ( model, Task.perform UploadRead (File.toString file) )

        UploadRead string ->
            let
                updateModel oldModel =
                    case fromJson string of
                        Just newModel ->
                            newModel

                        Nothing ->
                            oldModel
            in
            ( updateModel model, Cmd.none )



{--

  View: fixed footer menu containing
   naming textfield, download- and uploadButtons

--}


unorderdListStyle : List (Html.Styled.Attribute msg)
unorderdListStyle =
    -- listStyleType: none;
    [ css
        [ margin (px 0)
        , padding (px 0)
        , overflow hidden
        , zIndex (Css.int 10)
        , backgroundColor (rgb 240 240 240)
        , position fixed
        , left (px 0)
        , bottom (px 0)
        , height (px 50)
        , width (pct 100)
        ]
    ]


listItemStyle : List (Html.Styled.Attribute msg)
listItemStyle =
    [ css
        [ Css.float left
        , display block

        -- , color white
        , textAlign center
        , padding2 (px 14) (px 16)

        -- text-decoration: none;
        ]
    ]


view : Model -> Html Msg
view model =
    ul unorderdListStyle
        [ li listItemStyle [ copyJavaCommentsButton ]
        , li listItemStyle [ downloadButton model ]
        , li listItemStyle [ uploadButton ]
        ]


downloadButton : Model -> Html Msg
downloadButton model =
    button
        [ onClick DownloadJson ]
        [ text "Download" ]


uploadButton : Html Msg
uploadButton =
    button
        [ onClick UploadRequested ]
        [ text "Upload" ]


fileReader : File -> Cmd Msg
fileReader file =
    Task.perform UploadRead (File.toString file)


copyJavaCommentsButton : Html Msg
copyJavaCommentsButton =
    button
        [ onClick GenerateJavaComments ]
        [ text "Save to HTML" ]



{--

  Debugging

--}


debug : Model -> List (Html.Styled.Attribute State.Msg) -> Html State.Msg
debug model =
    -- Show decodedModel in a tree and visually check for diffs
    let
        decodingModel () =
            model
                |> toJson
                |> Debug.log "Debugmode: json"
                |> fromJson

        decodedModel () =
            case decodingModel () of
                Just newModel ->
                    newModel

                Nothing ->
                    Debug.log "Could not unpack decoded model, so using default model instead: " defaultModel
    in
    treeWithConditions (decodedModel ())



