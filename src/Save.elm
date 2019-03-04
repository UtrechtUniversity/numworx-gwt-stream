module Save exposing (Msg(..), basicTreeDecoder, copyJavaCommentsButton, downloadButton, encodeBasicTree, encodeModel, encodeTree, fromJson, modelDecoder, subscriptions, toJson, treeDecoder, update, uploadButton, view)

{--

  Converts a model as described in Tree.Core to and from JSON
    Model.highlightedBox is reset to Nothing, other values are passed on as is

  Legend: Update, Subscriptions, View, Encoding, Decoding

--}
-- import Tree.Draw as Draw exposing (treeWithConditions)

import Base64 exposing (decode)
import Color exposing (white)
import Css exposing (..)
import Debug exposing (log)
import Html.Styled exposing (Html, a, br, button, div, input, li, text, ul)
import Html.Styled.Attributes exposing (autofocus, class, css, href, id, placeholder, style, type_)
import Html.Styled.Events exposing (on, onClick, onInput)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)
import Ports exposing (JsonPortData, downloadToast, fileContentRead, fileSelected)
import Tree.Core as Tree exposing (..)
import Tree.State as State exposing (..)


type Msg
    = JsonSelected String
    | JsonRead JsonPortData
    | GenerateJavaComments


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonSelected inputBoxId ->
            ( model
            , fileSelected inputBoxId
            )

        JsonRead data ->
            let
                decoded64String =
                    -- This is unstable code
                    case Base64.decode <| String.dropLeft 13 data.contents of
                        Ok jsonModel ->
                            jsonModel

                        Err error ->
                            Debug.log "Couldn't base64 decode upload" error

                updateModel oldModel =
                    case fromJson decoded64String of
                        Just newModel ->
                            newModel

                        Nothing ->
                            oldModel
            in
            ( updateModel model
            , Cmd.none
            )

        GenerateJavaComments ->
            let
                newJavaComments =
                    --Debug.log "javaComments" <|
                    toJavaComment 0 model.tree
            in
            ( { model
                | javaComments = newJavaComments
              }
            , downloadToast newJavaComments
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead JsonRead



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
    a
        [ type_ "button"

        -- TODO removed encodeUri, does this still work?
        , href <| "data:text/plain;charset=utf-8," ++ toJson model

        -- Trigger modal upon downloading
        -- , downloadAs (model.flowchartName ++ ".flow")
        ]
        [ button [] [ text "Download" ] ]


uploadButton : Html Msg
uploadButton =
    let
        ownId =
            "jsonUploadButton"
    in
    div [ class "jsonWrapper" ]
        [ input
            [ type_ "file"
            , id ownId
            , on "change"
                (succeed <| JsonSelected ownId)
            ]
            []
        ]


copyJavaCommentsButton : Html Msg
copyJavaCommentsButton =
    button
        [ onClick GenerateJavaComments ]
        [ text "Convert to Java comments" ]



{--

  Encoding a Model

--}


toJson : State.Model -> String
toJson model =
    model
        |> encodeModel
        |> Encode.encode 4


encodeModel : State.Model -> Encode.Value
encodeModel model =
    Encode.object
        [ ( "flowchartName", Encode.string model.flowchartName )
        , ( "tree", encodeTree model.tree )
        , ( "currentId", Encode.int model.currentId )
        , ( "highlightedBox", Encode.string "Nothing" )
        , ( "precondition", Encode.string model.precondition )
        , ( "postcondition", Encode.string model.postcondition )
        , ( "javaComments", Encode.string model.javaComments )
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



{--

  Decoding a Model

--}


fromJson : String -> Maybe State.Model
fromJson json =
    let
        decodedResult =
            Decode.decodeString modelDecoder json
    in
    case decodedResult of
        Ok model ->
            Just <|
                Debug.log
                    "Decoded model without problems"
                    model

        Err model ->
            Debug.log
                ("Some decoding went wrong: "
                    ++ Debug.toString model
                )
                Nothing


modelDecoder : Decoder State.Model
modelDecoder =
    Decode.map7 State.Model
        (Decode.field "flowchartName" Decode.string)
        (Decode.field "tree" (lazy treeDecoder))
        (Decode.field "currentId" Decode.int)
        (Decode.field "highlightedBox" <| Decode.succeed Nothing)
        (Decode.field "precondition" <| Decode.string)
        (Decode.field "postcondition" <| Decode.string)
        (Decode.field "javaComments" <| Decode.string)


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
