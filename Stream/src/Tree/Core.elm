module Tree.Core exposing (BasicTree(..), Content, Id, Tree, continueRecursion, toStringRec, treeToJava)

{--

  Module containing underlying Treestructure and general functions

--}


type alias Tree =
    { id : Id
    , basicTree : BasicTree
    }


type BasicTree
    = Start Tree
    | End
    | Empty Tree
      -- invisible node for in If and While
    | Void
    | Statement Content Tree
      -- If text falseBranch trueBranch restOfTree
    | If Content Tree Tree Tree
      -- While text trueChild restOfTree
    | While Content Tree Tree
    | ForEach Content Tree Tree


type alias Content =
    String


type alias Id =
    Int


toStringRec : Tree -> String
toStringRec tree =
    -- For debugging only. NOT for anything else!
    -- Does print children
    case tree.basicTree of
        Start child ->
            String.concat [ "Start ", String.fromInt tree.id, " ", toStringRec child ]

        End ->
            String.concat [ "End ", String.fromInt tree.id ]

        Empty child ->
            String.concat [ "Empty ", String.fromInt tree.id, " ", toStringRec child ]

        Void ->
            String.concat [ "Void ", String.fromInt tree.id, " " ]

        Statement content child ->
            String.concat [ "Statement ", String.fromInt tree.id, " ", content, " ", toStringRec child ]

        If content child1 child2 child3 ->
            String.concat [ "If ", String.fromInt tree.id, " ", content, " ", toStringRec child1, toStringRec child2, toStringRec child3 ]

        While content child1 child2 ->
            String.concat [ "While ", String.fromInt tree.id, " ", content, " ", toStringRec child1, toStringRec child2 ]

        ForEach content child1 child2 ->
            String.concat [ "ForEach ", String.fromInt tree.id, " ", content, " ", toStringRec child1, toStringRec child2 ]


treeToJava : Int -> Tree -> String
treeToJava indent tree =
    let
        indentation =
            String.repeat indent "  "
    in
    case tree.basicTree of
        Start child ->
            treeToJava indent child

        End ->
            ""

        Empty child ->
            treeToJava indent child

        Void ->
            ""

        Statement content child ->
            String.concat
                [ indentation
                , "// "
                , String.replace "\n" ";\n// " content
                , "\n"

                -- Space to write down the syntacticly correct statement
                , indentation
                , "\n"
                , treeToJava indent child
                ]

        If content child1 child2 child3 ->
            String.concat
                [ indentation
                , "if("
                , String.replace "\n" "_" content
                , ") {\n"
                , treeToJava (indent + 1) child1
                , treeToJava (indent + 1) child2
                , indentation
                , "}\n"
                , treeToJava indent child3
                ]

        While content child1 child2 ->
            String.concat
                [ indentation
                , "while(" ++ String.replace "\n" "_" content
                , "){\n"
                , treeToJava (indent + 1) child1
                , indentation
                , "}\n"
                , treeToJava indent child2
                ]

        ForEach content child1 child2 ->
            String.concat
                [ indentation
                , "for(" ++ String.replace "\n" "_" content
                , "){\n"
                , treeToJava (indent + 1) child1
                , indentation
                , "}\n"
                , treeToJava indent child2
                ]



-- Useful in update functions:


continueRecursion : (Tree -> Tree) -> Tree -> Tree
continueRecursion function node =
    let
        helper =
            case node.basicTree of
                Start child ->
                    Start (function child)

                End ->
                    End

                Empty child ->
                    Empty (function child)

                Void ->
                    Void

                Statement content child ->
                    Statement content (function child)

                If content child1 child2 child3 ->
                    If content
                        (function child1)
                        (function child2)
                        (function child3)

                While content child1 child2 ->
                    While content
                        (function child1)
                        (function child2)

                ForEach content child1 child2 ->
                    ForEach content
                        (function child1)
                        (function child2)
    in
    { node | basicTree = helper }



{--
continueRecursionWithA : (( a, Tree ) -> ( a, Tree )) -> ( a, Tree ) -> ( a, Tree )
continueRecursionWithA function tuple =
    let
        ( a, node ) =
            tuple
    in
    case node.basicTree of
        Start child ->
            let
                ( updatedA, updatedChild ) =
                    function child
            in
            ( updatedA, { node | basicTree = Start updatedChild } )

        End ->
            ( a, Debug.log "Search: ended in " End )

        Empty child ->
            let
                ( updatedA, updatedChild ) =
                    function child
            in
            ( updatedA, { node | basicTree = Empty updatedChild } )

        Void ->
            Void

        Statement content child ->
            let
                ( updatedA, updatedChild ) =
                    function child
            in
            ( updatedA, { node | basicTree = Statement content updatedChild } )

        If content child1 child2 child3 ->
            let
                ( updatedA1, updatedChild1 ) =
                    function ( a, child1 )

                ( updatedA2, updatedChild2 ) =
                    function ( a, child2 )

                ( updatedA3, updatedChild3 ) =
                    function ( a, child3 )
            in
            -- TODO: How do I compare the updated a's of the children
            ( updatedA1, { node | basicTree = If content updatedA1 updatedA2 updatedA3 } )

        While content child1 child2 ->
            let
                ( updatedA1, updatedChild1 ) =
                    function ( a, child1 )

                ( updatedA2, updatedChild2 ) =
                    function ( a, child2 )
            in
            -- TODO: How do I compare the updated a's of the children
            ( updatedA1, { node | basicTree = While content updatedChild2 updatedChild2 } )
--}
