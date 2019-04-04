module Tree.State exposing (ChangeTree(..), Model, Msg(..), NodeType(..), defaultModel, init, update)

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

import Browser.Dom exposing (Error, blur)
import Debug exposing (log)
import Task exposing (attempt)
import Tree.Core exposing (..)


type alias Model =
    { flowchartName : String
    , tree : Tree
    , currentId : Id

    -- There can at most ONE highlighed box at the same time
    , highlightedBox : Maybe Id
    , precondition : Content
    , postcondition : Content
    , javaComments : String
    }


init : Model
init =
    { flowchartName = "Algorithm name"
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
    , precondition = ""
    , postcondition = ""
    , javaComments = ""
    }


defaultModel : Model
defaultModel =
    -- Only use in conjunction with Debug.log. This tree should never be able to exist
    { init
        | flowchartName = "Default Model"
        , tree = { id = 0, basicTree = End }
        , currentId = 0
    }



-- UPDATE


type Msg
    = UpdateName Content
    | UpdateContent Id Content
    | FillEmpty NodeType Id
    | ChangeTree ChangeTree Id
    | HighlightBox Id
    | DehighlightBox Id
    | KeyDown String Int
    | BlurResult (Result Error ())


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | flowchartName = newName }, Cmd.none )

        UpdateContent idToFind newContent ->
            ( updateContent newContent idToFind model, Cmd.none )

        FillEmpty newNodeType idToFind ->
            ( { model
                | tree = fillEmpty model.currentId newNodeType idToFind model.tree
                , currentId = model.currentId + 10

                -- 4 is the max number of new nodes created, 10 denotes a "generation"
              }
            , Cmd.none
            )

        ChangeTree operation id ->
            ( { model
                | tree = changeTree model.currentId operation id model.tree
                , currentId = model.currentId + 10
              }
            , Cmd.none
            )

        HighlightBox idHitbox ->
            ( { model
                | highlightedBox = highlightBox idHitbox model.highlightedBox
              }
            , Cmd.none
            )

        DehighlightBox idHitbox ->
            ( { model
                | highlightedBox = dehighlightBox idHitbox model.highlightedBox
              }
            , Cmd.none
            )

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
        { model | precondition = newContent }

    else if idToFind == 5 then
        { model | postcondition = newContent }

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
