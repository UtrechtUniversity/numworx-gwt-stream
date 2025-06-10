module Tree.Draw exposing (treeWithConditions)

{--

  Module containing helper functions to draw a tree as described in Core.elm

--}

import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout as Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text as Text exposing (Shape(..), Text, fromString, weight)
import Color exposing (Color, black, blue, darkGray, red, rgb255, rgba, white)
import Css exposing (auto, backgroundColor, borderColor, center, fontFamilies, left, overflow, pct, resize, textAlign)
import Html.Styled exposing (Html, div, fromUnstyled, input, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, cols, css, maxlength, placeholder, rows, style, type_, value, wrap)
import Html.Styled.Events exposing (onInput)
import Json.Decode as Json exposing (map)
import Tree.Core exposing (..)
import Tree.State exposing (..)


imposeAt : Anchor msg -> Collage msg -> Collage msg -> Collage msg
imposeAt anchor fore back =
    -- modification of "Collage.Layout.at" which leaves the center at 'back'
    -- In other words: stick something against something else, without moving the center
    impose
        (fore
            |> shift (anchor back)
        )
        back


imposePrime : Collage msg -> Collage msg -> Collage msg
imposePrime front back =
    -- modification to "Collage.Layout.at" for the hitbox. Fronts outline is used, and it is not overshadowed by the hitbox
    stack [ front, impose back front ]



{--

  Basic constants for drawing

--}


unit : Float
unit =
    10


gap : Collage msg
gap =
    spacer unit unit



-- Give '0' for a default arrow


arrow : Float -> Collage msg
arrow length =
    line
        (max length (unit * 3))
        |> traced defaultLineStyle
        |> rotate (pi / 2)
        |> imposeAt bottom (arrowTriangle |> align bottom)


arrowTriangle : Collage msg
arrowTriangle =
    triangle 15
        |> (filled <| uniform black)
        |> rotate pi


addBottomArrow : Float -> BasicTree -> Collage msg -> Collage msg
addBottomArrow reqLength child collage =
    -- Does not draw an arrow tip when void is the child
    -- Give '0' for a default arrow
    let
        length =
            max reqLength (unit * 3)
    in
    case child of
        Void ->
            [ collage |> align bottom
            , line length |> traced defaultLineStyle |> rotate (degrees 90) |> align top
            ]
                |> stack

        _ ->
            [ collage |> align bottom
            , arrow length |> align top
            ]
                |> stack


labelText : String -> Collage msg
labelText string =
    fromString string
        |> Text.size Text.small
        |> Text.shape Italic
        |> rendered


multilineEditableTextBox : Id -> NodeType -> String -> Int -> Int -> Int -> ( Html Msg, Float, Float )
multilineEditableTextBox id nodeType content minBoxWidth minBoxHeight maxBoxWidth =
    let
        characterWidth c =
            case c of
                '\t' ->
                    4

                '\n' ->
                    0

                _ ->
                    1

        -- Returns (currentWidth, maxWidth, height)
        boxDimensions s =
            case s of
                -- Note: The minimum width needs to be one bigger than the actual width, because otherwise a scrollbar would appear upon enter
                [] ->
                    ( 1, 1, 1 )

                c :: [] ->
                    if c == '\n' then
                        ( 1, 1, 2 )

                    else
                        ( characterWidth c + 1, characterWidth c + 1, 1 )

                c :: cs ->
                    let
                        ( cws, mws, hs ) =
                            boxDimensions cs
                    in
                    if c == '\n' then
                        ( 1, mws, hs + 1 )

                    else if cws + characterWidth c > maxBoxWidth then
                        ( characterWidth c, mws, hs + 1 )

                    else
                        ( cws + characterWidth c, max (cws + characterWidth c) mws, hs )

        ( _, wc, hc ) =
            boxDimensions
                -- Note: read the string backwards, so newlines indicate the *beginning* of a new line
                (List.reverse <| String.toList content)

        ( w, h ) =
            ( max minBoxWidth wc, max minBoxHeight hc )

        ( placeholderLabel, textAligning ) =
            case nodeType of
                StatementNode ->
                    ( "Statement", textAlign Css.left )

                IfNode ->
                    ( "If", textAlign Css.center )

                WhileNode ->
                    ( "While", textAlign Css.center )

                ForEachNode ->
                    ( "ForEach", textAlign Css.center )

                PreConditionNode ->
                    ( "Precondition", textAlign Css.left )

                PostConditionNode ->
                    ( "Postcondition", textAlign Css.left )

                FlowchartNameNode ->
                    ( "Algorithm name", textAlign Css.center )

        htmlTextArea =
            textarea
                [ wrap "hard"
                , cols w
                , rows h
                , css
                    [ overflow auto
                    , resize Css.none
                    -- , fontFamilies [  "courier", "monospace" ]
                    , backgroundColor (Css.rgba 0 0 0 0)
                    , borderColor (Css.rgba 0 0 0 0)
                    , textAligning
                    ]
                , placeholder placeholderLabel
                , value content
                , onInput <| UpdateContent <| id
                ]
                []
    in
    -- Offset the height so the bottom of the letters won't fall off
    ( htmlTextArea, toFloat w, (toFloat h) + 0.15 )



{--

  Draw standard shapes for nodes

--}


stubBox : String -> Collage Msg
stubBox stubText =
    -- Creates a collage node for "Start" and "End"
    -- Prints given string (presumably "Start" or "End")
    let
        shape =
            ellipse 50 25
                |> styled
                    ( uniform (rgb255 208 198 243)
                    , solid thin (uniform black)
                    )

        text =
            fromString stubText
                -- |> typeface
                |> rendered
    in
    [ text
    , shape
    ]
        |> stack
        |> name stubText


emptyBox : Id -> Collage Msg
emptyBox idEmpty =
    let
        menuGap =
            spacer unit unit

        options =
            List.intersperse menuGap
                [ boxNonEditable "statement" StatementNode
                    |> onClick (FillEmpty StatementNode idEmpty)
                , boxNonEditable "if" IfNode
                    |> onClick (FillEmpty IfNode idEmpty)
                , boxNonEditable "while" WhileNode
                    |> onClick (FillEmpty WhileNode idEmpty)
                , boxNonEditable "forEach" ForEachNode
                    |> onClick (FillEmpty ForEachNode idEmpty)
                ]
                |> horizontal
                |> Layout.center

        ( w, h ) =
            ( width options, height options )

        shape =
            rectangle (w + unit) (h + unit)
                |> styled
                    ( uniform (rgb255 255 202 255)
                    , dash thin (uniform darkGray)
                    )
    in
    [ options, shape ] |> stack


boxNonEditable : String -> NodeType -> Collage Msg
boxNonEditable label nodeType =
    let
        text =
            fromString label |> rendered

        w =
            max (width text) 40

        h =
            19

        shape =
            case nodeType of
                StatementNode ->
                    statementBoxShape w h

                IfNode ->
                    ifBoxShape w h

                WhileNode ->
                    loopBoxShape WhileNode w h

                ForEachNode ->
                    loopBoxShape ForEachNode w h

                _ ->
                    Debug.log "Tried to create non editable box for Precondition, Postcondition or FlowchartName. Drawing ellipse instead: " filled (uniform red) (ellipse 4 1)
    in
    [ text, shape ] |> stack


voidBox : Collage Msg
voidBox =
    spacer 0 0


statementBoxShape : Float -> Float -> Collage msg
statementBoxShape w h =
    rectangle (w + 2 * unit) (h + 3 * unit)
        |> styled
            ( uniform (rgb255 244 171 211)
            , solid thin (uniform black)
            )


statementBoxEditable : Id -> String -> Collage Msg
statementBoxEditable id label =
    let
        maxWidth =
            22

        ( minW, minH ) =
            ( 10, 1 )

        ( htmlTextArea, wta, hta ) =
            multilineEditableTextBox id StatementNode label minW minH maxWidth

        ( w, h ) =
            ( max minW wta * 9, max minH hta * 13 )

        htmlBox =
            html ( w, h * 1.3 ) <|
                toUnstyled htmlTextArea
    in
    [ htmlBox
    , statementBoxShape w h
    ]
        |> stack


ifBoxShape : Float -> Float -> Collage msg
ifBoxShape w h =
    let
        points =
            [ ( 0, h )
            , ( -(unit * 2), 0 )
            , ( 0, -h )
            , ( w, -h )
            , ( w + (unit * 2), 0 )
            , ( w, h )
            ]
    in
    polygon points
        |> styled
            ( uniform (rgb255 241 190 244)
            , solid thin (uniform black)
            )
        |> Layout.center


ifBoxEditable : Id -> String -> Collage Msg
ifBoxEditable id label =
    let
        maxWidth =
            23

        ( minW, minH ) =
            ( 10, 1 )

        ( htmlTextArea, wta, hta ) =
            multilineEditableTextBox id IfNode label minW minH maxWidth

        ( w, h ) =
            ( max minW wta * 9, max minH hta * 7.8 + 10 )

        htmlBox =
            html ( w, h * 2 ) <|
                toUnstyled htmlTextArea
    in
    stack
        [ htmlBox
            |> shift ( 0, -7 )
        , ifBoxShape w h
        ]


ifHelper : Model -> Tree -> String -> Tree -> Tree -> Tree -> Collage Msg
ifHelper model node text child1 child2 child3 =
    let
        ( leftPiece, rightPiece ) =
            ( drawTree model child1
            , drawTree model child2
            )

        maxHeight =
            max (height leftPiece) (height rightPiece) + unit * 5

        lineToBottom max tree =
            [ tree
            , max
                - height tree
                |> line
                |> traced defaultLineStyle
                |> rotate (pi / 2)
            ]
                |> vertical

        midPiece =
            [ leftPiece
                |> lineToBottom maxHeight
                |> name "leftPiece"
            , spacer widthMidGap 0
            , rightPiece
                |> lineToBottom maxHeight
                |> name "rightPiece"
            ]
                |> List.map (align top)
                |> horizontal
                |> shift ( -midLength / 2, 0 )

        -- The width of the arrows in the figure
        midLength =
            max (envelope Left rightPiece + envelope Right leftPiece + unit * 2) (width decoratedTextBox + 6 * unit)

        -- The empty gap in between the left and the right piece
        widthMidGap =
            midLength - envelope Left rightPiece - envelope Right leftPiece

        -- custom addBottomArrow that doesn't stack
        topArrow child dir length collage =
            case child of
                Void ->
                    collage |> imposeAt dir (line length |> traced defaultLineStyle |> rotate (degrees 90)) |> align top

                _ ->
                    collage |> imposeAt dir (arrow length) |> align top

        topArrows =
            midLength
                |> line
                |> traced defaultLineStyle
                |> imposeAt topRight
                    (labelText "true"
                        |> align bottomRight
                    )
                |> imposeAt topLeft
                    (labelText "false"
                        |> align bottomLeft
                    )
                |> align Layout.left
                |> beside Up (spacer 0 (height decoratedTextBox / 2 + 2 * unit))
                |> topArrow child1.basicTree Layout.left (height decoratedTextBox / 2 + 2 * unit)
                |> Layout.center
                |> topArrow child2.basicTree Layout.right (height decoratedTextBox / 2 + 2 * unit)
                |> Layout.center

        bottomLine =
            midLength
                |> line
                |> traced defaultLineStyle
                |> addSeparateBelowPlus model node

        decoratedTextBox =
            ifBoxEditable node.id text
                |> addOverlayMenu model.highlightedBox node
                |> imposeAt topLeft
                    (labelText "if"
                        |> align Layout.left
                    )
    in
    [ topArrows
    , midPiece
    , bottomLine
    ]
        |> vertical
        |> at top decoratedTextBox



{--

The While and the ForEach have the same structure, so I generalised them to 'loop'.

--}


loopBoxShape : NodeType -> Float -> Float -> Collage Msg
loopBoxShape nodeType w h =
    let
        boxColor =
            case nodeType of
                WhileNode ->
                    rgb255 181 199 245

                ForEachNode ->
                    rgb255 255 232 255

                _ ->
                    Debug.log "Tried to instantiate a loopBox of a node type that is not While or ForEach. Proceeding with white: " <| rgb255 0 0 0

        points =
            [ ( 0, h )
            , ( 0, -h )
            , ( (w + unit) / 2, -(h * 1.5) )
            , ( w + unit, -h )
            , ( w + unit, h )
            ]
    in
    polygon points
        |> styled
            ( uniform boxColor
            , solid thin (uniform black)
            )
        |> Layout.center


loopBoxEditable : NodeType -> Id -> String -> Collage Msg
loopBoxEditable nodeType id label =
    let
        maxCharacters =
            25

        ( minW, minH ) =
            ( 10, 1 )

        ( htmlTextArea, wta, hta ) =
            multilineEditableTextBox id nodeType label minW minH maxCharacters

        ( w, h ) =
            ( max minW wta * 9, max minH hta * 7.8 + 5 )

        htmlBox =
            html ( w, h * 2 ) <|
                toUnstyled htmlTextArea
    in
    [ htmlBox
    , loopBoxShape nodeType w h
    ]
        |> stack


loopHelper : NodeType -> Model -> Tree -> String -> Tree -> Tree -> Collage Msg
loopHelper nodeType model node text child1 child2 =
    let
        ( typeLabel, ( leftTag, bottomTag ) ) =
            case nodeType of
                WhileNode ->
                    -- The spaces in the tags are an ugly fix, I'm sorry
                    ( "while", ( "false   ", "   true" ) )

                ForEachNode ->
                    ( "for each", ( "done   ", "  repeat" ) )

                a ->
                    Debug.log ("Tried to create loopHelper with non-loop type: " ++ Debug.toString a ++ " continueing without change.") ( "report", ( "this", "please" ) )

        decoratedLoopBox =
            loopBoxEditable nodeType node.id text
                |> imposeAt right
                    (arrowTriangle
                        |> rotate (pi * 3 / 2)
                        |> align Layout.left
                    )
                |> addOverlayMenu model.highlightedBox node
                |> imposeAt topLeft
                    (labelText typeLabel
                        |> align bottom
                    )
                |> imposeAt Layout.left
                    (labelText leftTag
                        |> align bottomRight
                    )
                |> imposeAt bottom
                    (labelText bottomTag
                        |> align topLeft
                    )

        widthInner =
            max (width decoratedLoopBox + 4 * unit) (width <| drawTree model child1)

        inner =
            [ arrow (height decoratedLoopBox / 2)
            , drawTree model child1
            ]
                |> vertical

        ( topInner, leftInner, ( downInner, rightInner ) ) =
            -- Outerlines
            ( envelope Up inner + unit
            , max (envelope Left inner + unit) (width decoratedLoopBox / 2 + 2 * unit)
            , ( envelope Down inner
              , max (envelope Right inner + unit) (width decoratedLoopBox / 2 + 2 * unit)
              )
            )

        -- We need to place an extra add button at the end of the superpath. However, the hitbox must be drawn using small dimensions, so we use a line of lenght 1
        superPathOne =
            -- Start below child, then goes counterclockwise
            [ -- bridge the airgap around inner
              ( 0, -downInner )
            , ( rightInner, -downInner )
            , ( rightInner, topInner )
            , ( rightInner, topInner )
            , ( -leftInner, topInner )
            , ( -leftInner, -(downInner + 2 * unit) )
            , ( -1 * unit, -(downInner + 2 * unit) )
            , ( 0, -(downInner + 2 * unit) )
            ]
                |> path
                |> traced defaultLineStyle

        superPath =
            [ superPathOne

            -- Funfact: using a shape instead of a line creates a blinking hitbox
            , line 1
                |> traced invisible
                |> addSeparateBelowPlus model node
            ]
                |> vertical
    in
    [ superPath
    , inner
    ]
        |> stack
        |> at top decoratedLoopBox
        |> addBottomArrow 0 child2.basicTree



{--

  Draw the tree using earlier given shape functions

--}


drawTree : Model -> Tree -> Collage Msg
drawTree model node =
    case node.basicTree of
        Start child ->
            [ stubBox "Start"
                |> addOverlayMenu model.highlightedBox node
                |> addBottomArrow 0 child.basicTree
            , drawTree model child
            ]
                |> vertical

        End ->
            -- TODO simplify, remove vertical
            stubBox "End"
                |> addOverlayMenu model.highlightedBox node

        Empty child ->
            [ emptyBox node.id
                |> addOverlayMenu model.highlightedBox node
                |> addBottomArrow 0 child.basicTree
            , drawTree model child
            ]
                |> vertical

        Void ->
            voidBox

        Statement text child ->
            [ statementBoxEditable node.id text
                |> addOverlayMenu model.highlightedBox node
                |> addBottomArrow 0 child.basicTree
            , drawTree model child
            ]
                |> vertical

        If text child1 child2 child3 ->
            [ ifHelper model node text child1 child2 child3
                |> addBottomArrow 0 child3.basicTree
            , drawTree model child3
            ]
                |> vertical

        While text child1 child2 ->
            [ loopHelper WhileNode model node text child1 child2
            , drawTree model child2
            ]
                |> vertical

        ForEach text child1 child2 ->
            [ loopHelper ForEachNode model node text child1 child2
            , drawTree model child2
            ]
                |> vertical



{--

  Show an extra menu on mouse-over:
    - The ability to delete the boxes
    - The ability to add an empty node above or below it

--}


whitePlus : Collage msg
whitePlus =
    -- used in deleteBox and plusBox
    let
        ( w, h ) =
            ( unit * 3, unit * 10 )

        whiteRectangle =
            rectangle w h
                |> filled (uniform white)
    in
    [ whiteRectangle
    , whiteRectangle
        |> rotate (pi / 2)
    ]
        |> stack
        |> scale 0.1


plusBox : Collage msg
plusBox =
    [ whitePlus
    , circle (width whitePlus / 2 + 3)
        |> filled (uniform blue)
    ]
        |> stack


deleteBox : Collage msg
deleteBox =
    [ whitePlus
        |> rotate (pi / 4)
    , circle (width whitePlus / 2 + 3)
        |> filled (uniform red)
    ]
        |> stack


addHighlightOverlay : Tree -> Collage Msg -> Collage Msg
addHighlightOverlay node nodeBox =
    let
        deleteButton shape =
            shape
                |> imposeAt topRight
                    (deleteBox
                        |> onClick (ChangeTree Delete node.id)
                    )

        newAboveButton shape =
            shape
                |> imposeAt top
                    (plusBox
                        |> onClick (ChangeTree NewAbove node.id)
                    )

        newBelowButton shape =
            shape
                |> imposeAt bottom
                    (plusBox
                        |> onClick (ChangeTree NewBelow node.id)
                    )
    in
    case node.basicTree of
        Start _ ->
            nodeBox
                |> newBelowButton
                |> imposeAt Layout.right
                    (plusBox |> onClick (ConditionShow PreConditionNode))

        End ->
            nodeBox
                |> newAboveButton
                |> imposeAt Layout.right
                    (plusBox |> onClick (ConditionShow PostConditionNode))

        If _ _ _ _ ->
            nodeBox
                |> newAboveButton
                |> imposeAt Layout.right
                    (plusBox
                        |> onClick (ChangeTree NewTrue node.id)
                    )
                |> imposeAt Layout.left
                    (plusBox
                        |> onClick (ChangeTree NewFalse node.id)
                    )
                |> deleteButton

        While _ _ _ ->
            nodeBox
                |> newAboveButton
                |> imposeAt bottom
                    (plusBox
                        |> onClick (ChangeTree NewTrue node.id)
                    )
                |> deleteButton

        ForEach _ _ _ ->
            nodeBox
                |> newAboveButton
                |> imposeAt bottom
                    (plusBox
                        |> onClick (ChangeTree NewTrue node.id)
                    )
                |> deleteButton

        _ ->
            nodeBox
                |> newAboveButton
                |> newBelowButton
                |> deleteButton


addHitbox : Maybe Id -> Id -> Collage Msg -> Collage Msg
addHitbox highlightedBox id nodeBox =
    let
        ( w, h ) =
            ( width nodeBox, height nodeBox )

        hitbox =
            rectangle
                (w + unit * 2)
                (h + unit * 2)
                |> filled (uniform (rgba 0 0 0 0))

        trigger box =
            box
                |> (case highlightedBox of
                        Just highlightId ->
                            if id == highlightId then
                                -- In this case, a highlightingoverlay blocks this hitbox, causing the onMouseEnter to trigger multiple times
                                identity

                            else
                                -- 'always' is used to throw away the entrance point
                                onMouseEnter (always (HighlightBox id))

                        Nothing ->
                            onMouseEnter (always (HighlightBox id))
                   )
                |> onMouseLeave
                    (always (DehighlightBox id))
    in
    imposePrime (trigger nodeBox) (trigger hitbox)


addOverlayMenu : Maybe Id -> Tree -> Collage Msg -> Collage Msg
addOverlayMenu highlightedBox node nodeBox =
    nodeBox
        |> addHitbox highlightedBox node.id
        |> (case highlightedBox of
                Just highlightId ->
                    if node.id == highlightId then
                        addHighlightOverlay node

                    else
                        identity

                Nothing ->
                    identity
           )


addSeparateBelowPlus : Model -> Tree -> Collage Msg -> Collage Msg
addSeparateBelowPlus model node nodeBox =
    -- Similar to addOverlayMenu
    -- made for the if, while and foreach
    -- should one of these become a child of a while or foreach,
    --  then now there is a way to add a below child for it
    let
        addButton id =
            imposeAt bottom
                (plusBox
                    |> onClick (ChangeTree NewBelow node.id)
                )
    in
    nodeBox
        |> addHitbox model.highlightedBox node.id
        |> (case model.highlightedBox of
                Just highlightId ->
                    if node.id == highlightId then
                        addButton node.id

                    else
                        identity

                Nothing ->
                    identity
           )



{--

 Draw the flowchartName field

--}


flowchartNameBox : String -> Collage Msg
flowchartNameBox flowchartName =
    let
        -- TODO rewrtie to allow multiple lines
        htmlInputField =
            input
                [ autofocus True
                , placeholder "Flowchart name"
                , value flowchartName
                , maxlength 20
                , onInput UpdateName
                , css
                    [ Css.width (pct 100)
                    , fontFamilies [ "monaco", "monofur", "monospace" ]
                    , backgroundColor (Css.rgba 0 0 0 0)
                    , borderColor (Css.rgba 0 0 0 0)
                    , textAlign Css.center
                    ]
                ]
                []

        ( w, h ) =
            ( unit * 16, unit * 2 )

        htmlBox =
            html ( w, h ) <| toUnstyled htmlInputField

        flowchartNameBoxShape =
            rectangle (w + 2 * unit) (4 * unit)
                |> styled
                    ( uniform (rgb255 193 212 255)
                    , solid thin (uniform black)
                    )
    in
    [ htmlBox
    , flowchartNameBoxShape
    ]
        |> stack
        |> name "flowchartNameBox"



{--

  Draw pre- and postcondition notes

--}


stackTwo : Collage msg -> Collage msg -> Collage msg
stackTwo front back =
    -- Inline stacking in combination with |>
    -- Extends drawing range
    [ front, back ] |> stack


noteBox : Id -> String -> Collage Msg
noteBox id label =
    let
        ( minW, minH ) =
            ( 30, 4 )

        ( htmlTextArea, wta, hta ) =
            multilineEditableTextBox id nodeType label minW minH minW

        ( w, h ) =
            ( max minW wta * 5, max minH hta * 7.8 + 15 )

        nodeType =
            if id == 4 then
                PreConditionNode

            else if id == 5 then
                PostConditionNode

            else
                Debug.log "Tried to create non pre- or postcondition notebox. Using postcondition instead " PostConditionNode

        conditionType =
            if id == 4 then
                "Precondition"

            else
                "Postcondition"

        title =
            conditionType
                |> fromString
                |> weight Text.SemiBold
                |> rendered
                |> align topRight

        shape =
            [ ( w, h )
            , ( w, -h )
            , ( -w, -h )
            , ( -w, h - (unit * 2) )
            , ( -w + unit * 8, h )
            ]
                |> polygon
                |> styled
                    ( uniform (rgb255 220 237 248)
                    , solid thin (uniform black)
                    )
                |> align topRight
                |> shift ( unit, unit )
                |> impose title
                |> align topLeft
                |> shift ( -unit, unit * 2.5 )
                |> impose text

        text =
            html ( w * 2, h * 2 ) (toUnstyled htmlTextArea)
                |> align topLeft
    in
    shape
        |> name conditionType


addConditions : Model -> Collage Msg -> Collage Msg
addConditions model tree =
    let
        correctionCoordinates name =
            case locate name base tree of
                Just ( x, y ) ->
                    ( -x, -y )

                Nothing ->
                    Debug.log ("Coordinate not found " ++ name) ( 0, 0 )

        addPrecondition visible col =
            if visible then
                col
                    |> stackTwo
                        (noteBox 4 model.precondition.content
                            |> imposeAt topRight (deleteBox |> onClick (ConditionHide PreConditionNode))
                            |> align Layout.left
                        )
                    |> connect [ ( "Start", Layout.right ), ( "Precondition", Layout.left ) ] (dash verythin (uniform black))

            else
                col

        addPostcondition visible col =
            if visible then
                col
                    |> shift (correctionCoordinates "End")
                    |> shift ( 0, 2.6 * unit )
                    |> stackTwo
                        (noteBox 5 model.postcondition.content
                            |> align bottomLeft
                            |> shift ( 0, -3 * unit )
                            |> imposeAt topRight (deleteBox |> onClick (ConditionHide PostConditionNode))
                        )
                    |> connect [ ( "End", Layout.right ), ( "Postcondition", Layout.left ) ] (dash verythin (uniform black))

            else
                col
    in
    tree
        |> shift ( 6.5 * unit, -2.6 * unit )
        |> stackTwo
            (flowchartNameBox model.flowchartName
                |> align right
            )
        |> connect [ ( "Start", Layout.left ), ( "flowchartNameBox", Layout.right ) ] (dash verythin (uniform black))
        |> align right
        |> addPrecondition model.precondition.visible
        |> addPostcondition model.postcondition.visible
        -- The buttons aren't fully rendered because they are imposed. Therefore we create an artificial offset around the flowchart
        |> at Layout.right gap
        |> at Layout.right gap
        |> at Layout.top gap



{--

  Put everything together

--}


completeTree : Model -> Collage Msg
completeTree model =
    drawTree model model.tree
        |> at top gap
        |> at Layout.right gap


treeWithConditions : Model -> List (Html.Styled.Attribute Msg) -> Html Msg
treeWithConditions model msgAttributeHtmlList =
    div msgAttributeHtmlList
        [ completeTree model
            |> addConditions model
            |> svg
            |> fromUnstyled

        --, text ("Debug info, model.tree: " ++ toStringRec model.tree)
        ]
