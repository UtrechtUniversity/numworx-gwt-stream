module Save exposing (Msg(..), basicTreeDecoder, copyJavaCommentsButton, debug, downloadButton, encodeBasicTree, encodeModel, encodeTree, fromJson, modelDecoder, toJson, treeDecoder, update, uploadButton, view)

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
            , checkpoint <| toJson model
            )

        GenerateJavaComments ->
            ( model
            , downloadToast <| toJson model
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



{--

  Encoding a Model

--}


toJson : State.Model -> String
toJson model =
    model
        |> encodeEasterEgg
        |> Encode.encode 4



-- Tiny little easter egg, for the smart kids


encodeEasterEgg : State.Model -> Encode.Value
encodeEasterEgg model =
    Encode.object
        [ ( "_Easter_Egg", Encode.string "Nothing here" )
        , ( "model", encodeModel model )
        ]


encodeModel : State.Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "flowchartName", Encode.string model.flowchartName )
        , ( "tree", encodeTree model.tree )
        , ( "currentId", Encode.int model.currentId )
        , ( "highlightedBox", Encode.string "Nothing" )
        , ( "precondition", encodeCondition model.precondition )
        , ( "postcondition", encodeCondition model.postcondition )
        ]


encodeTree : Tree -> Encode.Value
encodeTree tree =
    Encode.object
        [ ( "id", Encode.int tree.id )
        , ( "basicTree", encodeBasicTree tree.basicTree )
        ]


encodeBasicTree : BasicTree -> Encode.Value
encodeBasicTree basicTree =
    let
        basicTreeCase =
            case basicTree of
                Start child ->
                    [ ( "basicTreeType"
                      , Encode.string "Start"
                      )
                    , ( "child"
                      , encodeTree child
                      )
                    ]

                End ->
                    [ ( "basicTreeType"
                      , Encode.string "End"
                      )
                    ]

                Empty child ->
                    [ ( "basicTreeType"
                      , Encode.string "Empty"
                      )
                    , ( "child"
                      , encodeTree child
                      )
                    ]

                Void ->
                    [ ( "basicTreeType"
                      , Encode.string "Void"
                      )
                    ]

                Statement content child ->
                    [ ( "basicTreeType"
                      , Encode.string "Statement"
                      )
                    , ( "content"
                      , Encode.string content
                      )
                    , ( "child"
                      , encodeTree child
                      )
                    ]

                If content child1 child2 child3 ->
                    [ ( "basicTreeType"
                      , Encode.string "If"
                      )
                    , ( "content"
                      , Encode.string content
                      )
                    , ( "child1"
                      , encodeTree child1
                      )
                    , ( "child2"
                      , encodeTree child2
                      )
                    , ( "child3"
                      , encodeTree child3
                      )
                    ]

                While content child1 child2 ->
                    [ ( "basicTreeType"
                      , Encode.string "While"
                      )
                    , ( "content"
                      , Encode.string content
                      )
                    , ( "child1"
                      , encodeTree child1
                      )
                    , ( "child2"
                      , encodeTree child2
                      )
                    ]

                ForEach content child1 child2 ->
                    [ ( "basicTreeType"
                      , Encode.string "ForEach"
                      )
                    , ( "content"
                      , Encode.string content
                      )
                    , ( "child1"
                      , encodeTree child1
                      )
                    , ( "child2"
                      , encodeTree child2
                      )
                    ]
    in
    Encode.object basicTreeCase


encodeCondition : Condition -> Encode.Value
encodeCondition condition =
    Encode.object
        [ ( "nodeType", encodeNodeType condition.nodeType )
        , ( "content", Encode.string condition.content )
        , ( "visible", Encode.bool condition.visible )
        ]


encodeNodeType : NodeType -> Encode.Value
encodeNodeType nodeType =
    case nodeType of
        StatementNode ->
            Encode.string "StatementNode"

        IfNode ->
            Encode.string "IfNode"

        WhileNode ->
            Encode.string "WhileNode"

        ForEachNode ->
            Encode.string "ForEachNode"

        PreConditionNode ->
            Encode.string "PreConditionNode"

        PostConditionNode ->
            Encode.string "PostConditionNode"

        FlowchartNameNode ->
            Encode.string "FlowchartNameNode"



{--

  Decoding a Model

--}


fromJson : String -> Maybe State.Model
fromJson json =
    let
        decodedResult =
            Decode.decodeString easterEggDecoder json
    in
    case decodedResult of
        Ok wrapper ->
            Just <|
                Debug.log
                    "Decoded model without problems"
                    wrapper.model

        Err wrapper ->
            Debug.log
                ("Some decoding went wrong: "
                    ++ Debug.toString wrapper
                )
                Nothing


type alias EasterEgg =
    { easterEgg : String
    , model : State.Model
    }


easterEggDecoder : Decoder EasterEgg
easterEggDecoder =
    Decode.map2 EasterEgg
        (Decode.field "_Easter_Egg" Decode.string)
        (Decode.field "model" modelDecoder)


modelDecoder : Decoder State.Model
modelDecoder =
    Decode.map6 State.Model
        (Decode.field "flowchartName" Decode.string)
        (Decode.field "tree" (lazy treeDecoder))
        (Decode.field "currentId" Decode.int)
        (Decode.field "highlightedBox" <| Decode.succeed Nothing)
        (Decode.field "precondition" conditionDecoder)
        (Decode.field "postcondition" conditionDecoder)


treeDecoder : () -> Decoder Tree
treeDecoder () =
    Decode.map2 Tree
        (Decode.field "id" Decode.int)
        (Decode.field "basicTree" (lazy basicTreeDecoder))


basicTreeDecoder : () -> Decoder BasicTree
basicTreeDecoder () =
    let
        basicTreeInfo : String -> Decoder BasicTree
        basicTreeInfo tag =
            case Debug.log "Now decoding: " tag of
                "Start" ->
                    Decode.map Start
                        (Decode.field "child" (lazy treeDecoder))

                "End" ->
                    Decode.succeed End

                "Empty" ->
                    Decode.map Empty
                        (Decode.field "child" (lazy treeDecoder))

                "Void" ->
                    Decode.succeed Void

                "Statement" ->
                    Decode.map2 Statement
                        (Decode.field "content" Decode.string)
                        (Decode.field "child" (lazy treeDecoder))

                "If" ->
                    Decode.map4 If
                        (Decode.field "content" Decode.string)
                        (Decode.field "child1" (lazy treeDecoder))
                        (Decode.field "child2" (lazy treeDecoder))
                        (Decode.field "child3" (lazy treeDecoder))

                "While" ->
                    Decode.map3 While
                        (Decode.field "content" Decode.string)
                        (Decode.field "child1" (lazy treeDecoder))
                        (Decode.field "child2" (lazy treeDecoder))

                "ForEach" ->
                    Decode.map3 ForEach
                        (Decode.field "content" Decode.string)
                        (Decode.field "child1" (lazy treeDecoder))
                        (Decode.field "child2" (lazy treeDecoder))

                a ->
                    Decode.fail <| "Unknown basicTree: " ++ a
    in
    Decode.field "basicTreeType" Decode.string
        |> andThen basicTreeInfo


conditionDecoder : Decoder Condition
conditionDecoder =
    Decode.map3 Condition
        (Decode.field "nodeType" <| nodeTypeDecoder)
        (Decode.field "content" <| Decode.string)
        (Decode.field "visible" <| Decode.bool)


nodeTypeDecoder : Decoder NodeType
nodeTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "StatementNode" ->
                        Decode.succeed StatementNode

                    "IfNode" ->
                        Decode.succeed IfNode

                    "WhileNode" ->
                        Decode.succeed WhileNode

                    "ForEachNode" ->
                        Decode.succeed ForEachNode

                    "PreConditionNode" ->
                        Decode.succeed PreConditionNode

                    "PostConditionNode" ->
                        Decode.succeed PostConditionNode

                    "FlowchartNameNode" ->
                        Decode.succeed FlowchartNameNode

                    _ ->
                        Decode.fail "Invalid NodeType"
            )
