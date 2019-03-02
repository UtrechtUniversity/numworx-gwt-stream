module Tree.State exposing (ChangeTree(..), ConditionType(..), FillEmpty(..), Model, Msg(..), defaultModel, init, unpackId, update)

{--

  Module containing state of tree as described in Tree.Core.elm and auxiliary variables
  Describes, inits and updates state

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


unpackId : Maybe Id -> Id
unpackId highlightedBox =
    -- highlightedBox is used in comparisons with a nodes own Id. Translating "Nothing" to -1 ensures these comparisons will always come up false.
    -- There's got to be a neater way to do this
    case highlightedBox of
        Just highlightedBoxId ->
            highlightedBoxId

        Nothing ->
            -1



-- UPDATE


type Msg
    = UpdateName Content
    | UpdateContent Id Content
    | FillEmpty FillEmpty Id
    | ChangeTree ChangeTree Id
    | HighlightBox Id
    | DehighlightBox Id
    | KeyDown String Int
    | BlurResult (Result Error ())
    | UpdateCondition ConditionType Content


type FillEmpty
    = AddStatement
    | AddIf
    | AddWhile
    | AddForEach


type ChangeTree
    = NewAbove
    | NewBelow
    | NewTrue
    | NewFalse
    | Delete


type ConditionType
    = Precondition
    | Postcondition


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName newName ->
            ( { model | flowchartName = newName }, Cmd.none )

        UpdateContent idToFind newContent ->
            ( { model | tree = updateContent newContent idToFind model.tree }, Cmd.none )

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

        UpdateCondition conditionType newText ->
            case conditionType of
                Precondition ->
                    ( { model | precondition = newText }, Cmd.none )

                Postcondition ->
                    ( { model | postcondition = newText }, Cmd.none )



{--

  Handeling of UpdateContent messages

--}


updateContent : Content -> Id -> Tree -> Tree
updateContent newContent idToFind node =
    let
        -- helper needs an argument, otherwise it gets pre-computed, which triggers
        --  the default case
        helper antiBugNode =
            case antiBugNode.basicTree of
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
    in
    if node.id == idToFind then
        { id = node.id, basicTree = helper node }

    else
        continueRecursion (updateContent newContent idToFind) node



{--

  Handeling of UpdateContent messages

--}


fillEmpty : Id -> FillEmpty -> Id -> Tree -> Tree
fillEmpty currentId newNodeType idToFind node =
    let
        onTheRightEmptyNode child =
            case newNodeType of
                AddStatement ->
                    Statement "" child

                AddIf ->
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

                AddWhile ->
                    While ""
                        { id = currentId
                        , basicTree =
                            Empty
                                { id = currentId + 1
                                , basicTree = Void
                                }
                        }
                        child

                AddForEach ->
                    ForEach ""
                        { id = currentId
                        , basicTree =
                            Empty
                                { id = currentId + 1
                                , basicTree = Void
                                }
                        }
                        child

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

        a ->
            Debug.log ("Tried to add an element below IfElse, While or ForEach. Id: " ++ String.fromInt currentNode.id ++ " instead doing nothing.") a


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
            Debug.log ("!!!Dehighlighted box " ++ String.fromInt oldId ++ "while nothing was highlighted") Nothing
