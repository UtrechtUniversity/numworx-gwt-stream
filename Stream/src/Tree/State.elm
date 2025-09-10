module Tree.State exposing (ChangeTree(..), Condition, Model, Msg(..), NodeType(..), defaultModel, init, modelToJava, update, toJson, fromJson, treeDecoder)

{--

  Module containing state of tree as described in Tree.Core.elm and auxiliary variables
  Describes, inits and updates state

  Note on (somewhat ugly) hardcoded id-numbers:
   0: Start node
   1: First Empty node
   2: End node
   3: Algorithm name box
   4: Precondition
   5: Postcondition
   10+: All generated nodes (mind, there are large gaps between these numbers)

--}

import Browser.Dom as Dom exposing (Error, blur)
import Debug exposing (log)
import Task exposing (attempt)
import Tree.Core exposing (..)
import Ports exposing (downloadToast, checkpoint)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


type alias Model =
    { flowchartName : String
    , tree : Tree
    , currentId : Id

    -- There can at most ONE highlighed box at the same time
    , highlightedBox : Maybe Id
    , precondition : Condition
    , postcondition : Condition
    }


init : Model
init =
    { flowchartName = ""
    , tree =
        { id = 0
        , basicTree =
            Start
                { id = 2
                , basicTree =
                    Empty
                        { id = 1, basicTree = End }
                }
        }
    , currentId = 10
    , highlightedBox = Nothing
    , precondition = { nodeType = PreConditionNode, content = "", visible = False }
    , postcondition = { nodeType = PostConditionNode, content = "", visible = False }
    }


defaultModel : Model
defaultModel =
    -- Only use in conjunction with Debug.log. This tree should never be able to exist
    { init
        | flowchartName = "Default Model"
        , tree = { id = 0, basicTree = End }
        , currentId = 0
    }


modelToJava : Model -> String
modelToJava model =
    -- We need to replace '<' so the comments aren't parsed as HTML-tags
    String.replace "<" "&lt;" <|
        String.concat <|
            List.intersperse "\n"
                [ "/**"
                , " * <P> Initial:" ++ conditionToJava model.precondition
                , " * <P> Final:" ++ conditionToJava model.postcondition
                , " */"
                , "public void " ++ String.replace " " "_" model.flowchartName ++ "(){"
                , treeToJava 1 model.tree ++ "}"
                ]


conditionToJava : Condition -> String
conditionToJava condition =
    String.replace "\n" "\n *       " condition.content



-- UPDATE


type Msg
    = UpdateName Content
    | UpdateContent Id Content
    | FillEmpty NodeType Id
    | ChangeTree ChangeTree Id
    | HighlightBox Id
    | DehighlightBox Id
    | KeyDown String Int
    | BlurResult (Result Dom.Error ())
    | ConditionHide NodeType
    | ConditionShow NodeType
    | Checkpoint


type NodeType
    = StatementNode
    | IfNode
    | WhileNode
    | ForEachNode
    | PreConditionNode
    | PostConditionNode
    | FlowchartNameNode


type ChangeTree
    = NewAbove
    | NewBelow
    | NewTrue
    | NewFalse
    | Delete


type alias Condition =
    { nodeType : NodeType
    , content : Content
    , visible : Bool
    }


setConditionContent : Content -> Condition -> Condition
setConditionContent newContent condition =
    -- Elm does not allow struct-in-struct changes, so this function works as some syntactic sugar
    { condition | content = newContent }


setVisibleContent : Bool -> Condition -> Condition
setVisibleContent bool condition =
    { condition | visible = bool }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Checkpoint -> 
            ( model, checkpoint <| toJson model) 
    
        UpdateName newName ->
            update Checkpoint ({ model | flowchartName = newName})

        UpdateContent idToFind newContent ->
            update Checkpoint ( updateContent newContent idToFind model )

        FillEmpty newNodeType idToFind ->
            update Checkpoint ( { model
                | tree = fillEmpty model.currentId newNodeType idToFind model.tree
                , currentId = model.currentId + 10

                -- 4 is the max number of new nodes created, 10 denotes a "generation"
              })

        ChangeTree operation id ->
            update Checkpoint ( { model
                | tree = changeTree model.currentId operation id model.tree
                , currentId = model.currentId + 10
              })

        HighlightBox idHitbox ->
            update Checkpoint ( { model
                | highlightedBox = highlightBox idHitbox model.highlightedBox
              })

        DehighlightBox idHitbox ->
            update Checkpoint ( { model
                | highlightedBox = dehighlightBox idHitbox model.highlightedBox
              })

        KeyDown domId key ->
            if key == 13 then
                ( model
                , --Debug.log "Our dark magic is summoned upon!" <|
                  attempt BlurResult (blur domId)
                )

            else
                ( model, Cmd.none )

        BlurResult result ->
            case result of
                Ok _ ->
                    --Debug.log "Blur succesfull!"
                    ( model, Cmd.none )

                Err _ ->
                    --Debug.log "Blur failed!"
                    ( model, Cmd.none )

        ConditionHide nodeType ->
            case nodeType of
                PreConditionNode ->
                    update Checkpoint ( { model | precondition = setVisibleContent False model.precondition })

                PostConditionNode ->
                    update Checkpoint ( { model | postcondition = setVisibleContent False model.postcondition })

                _ ->
                    --Debug.log "ConditionHide on non-condition type!"
                    ( model, Cmd.none )

        ConditionShow nodeType ->
            case nodeType of
                PreConditionNode ->
                    update Checkpoint ( { model | precondition = setVisibleContent True model.precondition })

                PostConditionNode ->
                    update Checkpoint ( { model | postcondition = setVisibleContent True model.postcondition })

                _ ->
                    --Debug.log "ConditionShow on non-condition type!"
                    ( model, Cmd.none )



{--

  Handeling of UpdateContent messages

--}


updateContent : Content -> Id -> Model -> Model
updateContent newContent idToFind model =
    let
        -- helper needs an argument, otherwise it gets pre-computed, which triggers
        --  the default case
        helper node =
            case node.basicTree of
                Statement content child ->
                    Statement newContent child

                If content child1 child2 child3 ->
                    If newContent child1 child2 child3

                While content child1 child2 ->
                    While newContent child1 child2

                ForEach content child1 child2 ->
                    ForEach newContent child1 child2

                a ->
                    Debug.log ("Tried to update content of non-content node" ++ Debug.toString a ++ " With Id: " ++ String.fromInt node.id ++ " instead doing nothing.") Void

        treeRecursion node =
            if node.id == idToFind then
                { id = node.id, basicTree = helper node }

            else
                continueRecursion treeRecursion node
    in
    if idToFind == 3 then
        { model | flowchartName = newContent }

    else if idToFind == 4 then
        { model | precondition = setConditionContent newContent model.precondition }

    else if idToFind == 5 then
        { model | postcondition = setConditionContent newContent model.postcondition }

    else
        { model | tree = treeRecursion model.tree }



{--

  Handeling of UpdateContent messages

--}


fillEmpty : Id -> NodeType -> Id -> Tree -> Tree
fillEmpty currentId newNodeType idToFind node =
    let
        onTheRightEmptyNode child =
            case newNodeType of
                StatementNode ->
                    Statement "" child

                IfNode ->
                    If ""
                        { id = currentId
                        , basicTree =
                            Empty
                                { id = currentId + 1, basicTree = Void }
                        }
                        { id = currentId + 2
                        , basicTree =
                            Empty
                                { id = currentId + 3, basicTree = Void }
                        }
                        child

                WhileNode ->
                    While ""
                        { id = currentId
                        , basicTree =
                            Empty
                                { id = currentId + 1
                                , basicTree = Void
                                }
                        }
                        child

                ForEachNode ->
                    ForEach ""
                        { id = currentId
                        , basicTree =
                            Empty
                                { id = currentId + 1
                                , basicTree = Void
                                }
                        }
                        child

                _ ->
                    Debug.log "Tried to instantiate a Precondition, Postcondition or FlowchartName in function 'fillEmpty'. Instantiated Void instead" Void

        helper currentNode =
            case currentNode of
                Empty child ->
                    onTheRightEmptyNode child

                a ->
                    Debug.log ("Inserting something on non-Empty node " ++ Debug.toString a ++ " with id: " ++ String.fromInt node.id ++ " instead doing nothing.") currentNode
    in
    if node.id == idToFind then
        { node | basicTree = helper node.basicTree }

    else
        continueRecursion
            (fillEmpty currentId newNodeType idToFind)
            node



{--

    Handeling of ChangeTree messages

--}


changeTree : Id -> ChangeTree -> Id -> Tree -> Tree
changeTree currentId operation idToFind node =
    let
        onTheRightNode : Tree -> Tree
        onTheRightNode currentNode =
            case operation of
                NewAbove ->
                    newAbove currentId currentNode

                NewBelow ->
                    { node | basicTree = newBelow currentId currentNode }

                NewTrue ->
                    { node | basicTree = newTrue currentId currentNode }

                NewFalse ->
                    { node | basicTree = newFalse currentId currentNode }

                Delete ->
                    delete currentNode
    in
    if node.id == idToFind then
        onTheRightNode node

    else
        continueRecursion (changeTree currentId operation idToFind) node


newAbove : Id -> Tree -> Tree
newAbove currentId node =
    case node.basicTree of
        Start child ->
            Debug.log ("Tried to add an element before Start. Id: " ++ String.fromInt node.id ++ " instead doing nothing.") { id = -2, basicTree = Start child }

        Void ->
            Debug.log ("Tried to add an element before Void. Id: " ++ String.fromInt node.id ++ " instead doing nothing.") { id = -3, basicTree = Void }

        _ ->
            { id = currentId, basicTree = Empty node }


newBelow : Id -> Tree -> BasicTree
newBelow currentId currentNode =
    case currentNode.basicTree of
        Start child ->
            Start
                { id = currentId
                , basicTree = Empty child
                }

        End ->
            Debug.log ("Tried to add an element below End. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") End

        Empty child ->
            Empty
                { id = currentId
                , basicTree = Empty child
                }

        Void ->
            Debug.log ("Tried to add an element below Void. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") Void

        Statement content child ->
            Statement content
                { id = currentId
                , basicTree = Empty child
                }

        If content leftChild rightChild belowChild ->
            If content
                leftChild
                rightChild
                { id = currentId
                , basicTree = Empty belowChild
                }

        While content innerChild belowChild ->
            While content innerChild { id = currentId, basicTree = Empty belowChild }

        ForEach content innerChild belowChild ->
            ForEach content innerChild { id = currentId, basicTree = Empty belowChild }


newTrue : Id -> Tree -> BasicTree
newTrue currentId currentNode =
    case currentNode.basicTree of
        If content child1 child2 child3 ->
            If content
                child1
                { id = currentId
                , basicTree = Empty child2
                }
                child3

        While content child1 child2 ->
            While content
                { id = currentId
                , basicTree = Empty child1
                }
                child2

        ForEach content child1 child2 ->
            ForEach content
                { id = currentId
                , basicTree = Empty child1
                }
                child2

        a ->
            Debug.log ("Tried to give a newTrue child to: " ++ Debug.toString a ++ " with Id: " ++ String.fromInt currentNode.id ++ " instead using: ") Void


newFalse : Id -> Tree -> BasicTree
newFalse currentId currentNode =
    case currentNode.basicTree of
        If content child1 child2 child3 ->
            If content
                { id = currentId
                , basicTree = Empty child1
                }
                child2
                child3

        a ->
            Debug.log ("Tried to give a newFalse child to: " ++ Debug.toString a ++ " with Id: " ++ String.fromInt currentNode.id ++ " instead using: ") Void


delete : Tree -> Tree
delete currentNode =
    case currentNode.basicTree of
        Start child ->
            Debug.log ("Tried to delete Start. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") { id = -2, basicTree = Start child }

        End ->
            Debug.log ("Tried to delete End. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") { id = -3, basicTree = End }

        Empty child ->
            child

        Void ->
            Debug.log ("Tried to delete Void. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") { id = -4, basicTree = Void }

        Statement content child ->
            child

        If content child1 child2 child3 ->
            child3

        While content child1 child2 ->
            child2

        ForEach content child1 child2 ->
            child2



{--

  Handeling of (de)HighlightBox messages

--}


highlightBox : Id -> Maybe Id -> Maybe Id
highlightBox newId currentId =
    case currentId of
        Just id ->
            if newId == id then
                Nothing

            else
                Just newId

        Nothing ->
            Just newId


dehighlightBox : Id -> Maybe Id -> Maybe Id
dehighlightBox oldId currentId =
    case currentId of
        Just id ->
            if oldId == id then
                Nothing

            else
                Debug.log ("!!!Dehighlighted box " ++ String.fromInt oldId ++ " while the highlightedBox was " ++ String.fromInt id) Nothing

        Nothing ->
            Debug.log ("!!!Dehighlighted box " ++ String.fromInt oldId ++ " while nothing was highlighted") Nothing

            
{--

  Encoding a Model

--}


toJson : Model -> String
toJson model =
    model
        |> encodeEasterEgg
        |> Encode.encode 4



-- Tiny little easter egg, for the smart kids


encodeEasterEgg : Model -> Encode.Value
encodeEasterEgg model =
    Encode.object
        [ ( "_Easter_Egg", Encode.string "Nothing here" )
        , ( "model", encodeModel model )
        ]


encodeModel : Model -> Encode.Value
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


fromJson : String -> Maybe Model
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
    , model : Model
    }


easterEggDecoder : Decoder EasterEgg
easterEggDecoder =
    Decode.map2 EasterEgg
        (Decode.field "_Easter_Egg" Decode.string)
        (Decode.field "model" modelDecoder)


modelDecoder : Decoder Model
modelDecoder =
    Decode.map6 Model
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
            
            