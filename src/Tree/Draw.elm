module Tree.Draw exposing (treeWithConditions, offsetLeft)

{--

  Module containing helper functions to draw a tree as described in Core.elm

--}

import Collage exposing (..)
import Collage.Events exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Collage.Text as Text exposing (Shape(..), Text, fromString, weight)
import Color exposing (Color, rgb, rgba, black, blue, darkGray, red, white)
import Html exposing (..)
import Html.Attributes exposing (cols, maxlength, placeholder, rows, style, type_, wrap)
import Html.Events exposing (onInput)
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



{--

  Basic constants for drawing

--}


unit : Float
unit =
    10


gap : Collage msg
gap =
    spacer unit unit


arrow : Collage msg
arrow =
    vertical
        [ line (unit * 2.5)
            |> traced defaultLineStyle
            |> rotate (pi / 2)
        , arrowTriangle
        ]


arrowTriangle : Collage msg
arrowTriangle =
    triangle 15
        |> (filled <| uniform black)
        |> rotate pi


collageWithTopArrow : Collage msg -> Collage msg
collageWithTopArrow collage =
    --
    [ collage |> align top
    , arrow |> align bottom
    ]
        |> stack


labelText : String -> Collage msg
labelText string =
    fromString string
        |> Text.size Text.small
        |> Text.shape Italic
        |> rendered


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    -- Needed for catching an "enter"/"return" to defocus
    Html.Events.on "keydown" (Json.map tagger Html.Events.keyCode)


textBox : Id -> FillEmpty -> String -> Int -> Html Msg
textBox id newNodeType label maxCharacters =
    let
        placeholderLabel =
            case newNodeType of
                AddStatement ->
                    "Statement"

                AddIf ->
                    "If"

                AddWhile ->
                    "While"

                AddForEach ->
                    "ForEach"
    in
    input
        [ placeholder placeholderLabel
        , maxlength <| max maxCharacters <| String.length label
        , onInput (UpdateContent id)
        , Html.Attributes.id <| String.fromInt id
        , onKeyDown <| KeyDown <| String.fromInt id
        , Html.Attributes.value label

        --, Html.Attributes.autofocus True
        , style "width" "100%"

            -- , ( "font-family", "'monaco', 'monofur', monospace" )
            -- , ( "background-color" "rgba(0, 0, 0, 0)" )
            -- , ( "border-color" "rgba(0, 0, 0, 0)" )
            -- , ( "text-align" "center" )
            -- ]
        ]
        []



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
                    ( uniform (rgb 208 198 243)
                    , solid thin (uniform black)
                    )

        text =
            fromString stubText
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
                [ boxNonEditable "statement" AddStatement
                    |> onClick (FillEmpty AddStatement idEmpty)
                , boxNonEditable "if" AddIf
                    |> onClick (FillEmpty AddIf idEmpty)
                , boxNonEditable "while" AddWhile
                    |> onClick (FillEmpty AddWhile idEmpty)
                , boxNonEditable "forEach" AddForEach
                    |> onClick (FillEmpty AddForEach idEmpty)
                ]
                |> horizontal
                |> center

        ( w, h ) =
            ( width options, height options )

        shape =
            rectangle (w + unit) (h + unit)
                |> styled
                    ( uniform (rgb 255 202 255)
                    , dash thin (uniform darkGray)
                    )
    in
    [ options, shape ] |> stack


boxNonEditable : String -> FillEmpty -> Collage Msg
boxNonEditable label nodeType =
    let
        text =
            fromString label |> rendered

        w =
            max (width text) 40

        shape =
            case nodeType of
                AddStatement ->
                    statementBoxShape w

                AddIf ->
                    ifBoxShape w

                AddWhile ->
                    whileBoxShape w

                AddForEach ->
                    forEachBoxShape w
    in
    [ text, shape ] |> stack


voidBox : Collage Msg
voidBox =
    spacer 0 0


statementBoxShape : Float -> Collage msg
statementBoxShape w =
    rectangle (w + 2 * unit) (4 * unit)
        |> styled
            ( uniform (rgb 244 171 211)
            , solid thin (uniform black)
            )



{--
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
--}


statementBoxEditable : Id -> String -> Collage Msg
statementBoxEditable id label =
    let
        ( w, h ) =
            ( unit * 18, unit * 2 )

        htmlBox =
            html ( w, h ) <|
                textBox id AddStatement label 22
    in
    [ htmlBox
    , statementBoxShape w
    ]
        |> stack


ifBoxShape : Float -> Collage msg
ifBoxShape w =
    let
        points =
            [ ( 0, unit * 2 )
            , ( -(unit * 2), 0 )
            , ( 0, -(unit * 2) )
            , ( w, -(unit * 2) )
            , ( w + (unit * 2), 0 )
            , ( w, unit * 2 )
            ]
    in
    polygon points
        |> styled
            ( uniform (rgb 241 190 244)
            , solid thin (uniform black)
            )
        |> center


ifBoxEditable : Id -> String -> Collage Msg
ifBoxEditable id label =
    -- Copyright claim to T. Steenvoorden :sweatsmile:
    let
        maxCharacters =
            25

        characterWidth =
            min maxCharacters <| max (String.length label) 6

        w =
            max (unit * 0.85 * toFloat characterWidth) 70

        htmlBox =
            html ( w, 2 * unit ) <|
                textBox id AddIf label maxCharacters
    in
    stack
        [ htmlBox
        , ifBoxShape w
        ]


ifHelper : Model -> Tree -> String -> Tree -> Tree -> Tree -> Collage Msg
ifHelper model node text child1 child2 child3 =
    let
        ( leftPiece, rightPiece ) =
            ( drawTree model child1, drawTree model child2 )

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

        widthMidGap =
            max (unit * 20 - envelope Left rightPiece - envelope Right leftPiece) (unit * 5)

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

        midLength =
            envelope Left rightPiece
                + envelope Right leftPiece
                + widthMidGap

        horizontalLine =
            midLength
                |> line
                |> traced defaultLineStyle
    in
    [ horizontalLine
        |> imposeAt topRight
            (labelText "true"
                |> align bottomRight
            )
        |> imposeAt topLeft
            (labelText "false"
                |> align bottomLeft
            )
    , midPiece
    , horizontalLine
    ]
        |> vertical
        |> at top
            (ifBoxEditable node.id text
                |> addOverlayMenu model.highlightedBox node
                |> imposeAt topLeft
                    (labelText "if"
                        |> align left
                    )
            )



{--

  The While and the ForEach have the same structure, but differ in their main box shape. First the editable boxes will be made, then a generalised "loopHelper" to create the structure around them.

--}


whileBoxShape : Float -> Collage Msg
whileBoxShape w =
    let
        points =
            [ ( 0, unit * 1.5 )
            , ( 0, -(unit * 1.5) )
            , ( (w + unit) / 2, -(unit * 2.5) )
            , ( w + unit, -(unit * 1.5) )
            , ( w + unit, unit * 1.5 )
            ]
    in
    polygon points
        |> styled
            ( uniform (rgb 181 199 245)
            , solid thin (uniform black)
            )
        |> center


whileBoxEditable : Id -> String -> Collage Msg
whileBoxEditable id label =
    let
        maxCharacters =
            25

        characterWidth =
            min maxCharacters <| max (String.length label) 5

        w =
            max (unit * 0.85 * toFloat characterWidth) 70

        htmlBox =
            html ( w, 2 * unit ) <|
                textBox id AddWhile label maxCharacters
    in
    [ htmlBox
    , whileBoxShape w
    ]
        |> stack


forEachBoxShape : Float -> Collage Msg
forEachBoxShape w =
    let
        points =
            [ ( 0, unit * 1.5 )
            , ( 0, -(unit * 1.5) )
            , ( (w + unit) / 2, -(unit * 2.5) )
            , ( w + unit, -(unit * 1.5) )
            , ( w + unit, unit * 1.5 )
            ]
    in
    polygon points
        |> styled
            ( uniform (rgb 255 232 255)
            , solid thin (uniform black)
            )
        |> center


forEachBoxEditable : Id -> String -> Collage Msg
forEachBoxEditable id label =
    let
        maxCharacters =
            25

        characterWidth =
            min maxCharacters <| max (String.length label) 5

        w =
            max (unit * 0.85 * toFloat characterWidth) 70

        htmlBox =
            html ( w, 2 * unit ) <|
                textBox id AddForEach label maxCharacters
    in
    [ htmlBox
    , forEachBoxShape w
    ]
        |> stack


loopHelper : FillEmpty -> Model -> Tree -> String -> Tree -> Tree -> Collage Msg
loopHelper nodeType model node text child1 child2 =
    let
        ( loopBox, typeLabel, (leftTag, bottomTag )) =
            case nodeType of
                AddWhile ->
                    -- The spaces in the tags are an ugly fix, I'm sorry
                    ( whileBoxEditable, "while", ("false   ", "   true") )

                AddForEach ->
                    ( forEachBoxEditable, "for each", ("done   ", "  repeat") )

                a ->
                    Debug.log ("Tried to create loopHelper with non-loop type: " ++ Debug.toString a ++ " continueing without change.") (whileBoxEditable, "report", ("please", "this"))

        decoratedLoopBox =
            loopBox node.id text
                |> imposeAt right
                    (arrowTriangle
                        |> rotate (pi * 3 / 2)
                        |> align left
                    )
                |> addOverlayMenu model.highlightedBox node
                |> imposeAt topLeft
                    (labelText typeLabel
                        |> align bottom
                    )
                |> imposeAt left
                    (labelText leftTag
                        |> align bottomRight
                    )
                |> imposeAt bottom
                    (labelText bottomTag
                        |> align topLeft
                    )

        widthInner =
            max (20 * unit) (width <| drawTree model child1)

        inner =
            [ [ drawTree model child1
              , line unit
                    |> traced defaultLineStyle
                    |> rotate (pi / 2)
              ]
                |> vertical
            , spacer widthInner 0
            ]
                |> stack

        ( topInner, leftInner, (downInner, rightInner )) =
            -- Outerlines
            ( envelope Up inner + unit
            , envelope Left inner + unit
            , (envelope Down inner + unit
            , envelope Right inner + unit
            ))

        superPath =
            -- Start below child, then goes counterclockwise
            [ -- bridge the airgap around inner
              ( 0, -downInner + unit )
            , ( 0, -downInner )
            , ( rightInner, -downInner )
            , ( rightInner, topInner )
            , ( rightInner, topInner )
            , ( -leftInner, topInner )
            , ( -leftInner, -(downInner + 2 * unit) )
            , ( 0, -(downInner + 2 * unit) )
            ]
                |> path
                |> traced defaultLineStyle
    in
    [ superPath
    , inner
    ]
        |> stack
        |> at top decoratedLoopBox



{--

  Draw the tree using earlier given shape functions

--}


drawTree : Model -> Tree -> Collage Msg
drawTree model node =
    case node.basicTree of
        Start child ->
            [ stubBox "Start"
                |> addOverlayMenu model.highlightedBox node
            , drawTree model child
            ]
                |> vertical

        End ->
            [ collageWithTopArrow
                (stubBox "End"
                    |> addOverlayMenu model.highlightedBox node
                )
            ]
                |> vertical

        Empty child ->
            [ collageWithTopArrow
                (emptyBox node.id
                    |> addOverlayMenu model.highlightedBox node
                )
            , drawTree model child
            ]
                |> vertical

        Void ->
            voidBox

        Statement text child ->
            [ collageWithTopArrow
                (statementBoxEditable node.id text
                    |> addOverlayMenu model.highlightedBox node
                )
            , drawTree model child
            ]
                |> vertical

        If text child1 child2 child3 ->
            [ collageWithTopArrow
                (ifHelper model node text child1 child2 child3)
            , drawTree model child3
            ]
                |> vertical

        While text child1 child2 ->
            [ collageWithTopArrow
                (loopHelper AddWhile model node text child1 child2)
            , drawTree model child2
            ]
                |> vertical

        ForEach text child1 child2 ->
            [ collageWithTopArrow
                (loopHelper AddForEach model node text child1 child2)
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

        End ->
            nodeBox
                |> newAboveButton

        If _ _ _ _ ->
            nodeBox
                |> newAboveButton
                |> imposeAt right
                    (plusBox
                        |> onClick (ChangeTree NewTrue node.id)
                    )
                |> imposeAt left
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
            rectangle (w + unit * 2) (h + unit * 2)
                |> filled (uniform (rgba 250 20 20 0.1))

        trigger box =
            box
                |> (if id == unpackId highlightedBox then
                        -- In this case, a highlightingoverlay blocks this hitbox, causing the onMouseEnter to trigger multiple times
                        identity
                    else
                        -- 'always' is used to throw away the entrance point
                        onMouseEnter (always (HighlightBox id))
                   )
                |> onMouseLeave
                    (always (DehighlightBox id))
    in
    impose (trigger hitbox) nodeBox


addOverlayMenu : Maybe Id -> Tree -> Collage Msg -> Collage Msg
addOverlayMenu highlightedBox node nodeBox =
    nodeBox
        |> addHitbox highlightedBox node.id
        |> (if node.id == unpackId highlightedBox then
                addHighlightOverlay node
            else
                identity
           )



{--

  Draw pre- and postcondition notes

--}


multilineEditableTextBox : ConditionType -> String -> Html Msg
multilineEditableTextBox conditionType label =
    let
        styling =
            style "overflow" "auto"
                -- [ ( "overflow", "auto" )
                -- , ( "resize", "none" )
                -- , ( "font-family", "'monaco', 'monofur', monospace" )
                -- , ( "background-color", "rgba(0, 0, 0, 0)" )
                -- , ( "border-color", "rgba(0, 0, 0, 0)" )
                -- ]
    in
    textarea
        [ wrap "hard"
        , cols 30
        , rows 4
        , styling
        , placeholder <| Debug.toString conditionType
        , Html.Attributes.value label
        , onInput <| UpdateCondition conditionType
        ]
        []


stackTwo : Collage msg -> Collage msg -> Collage msg
stackTwo front back =
    -- Inline stacking in combination with |>
    -- Extends drawing range
    [ front, back ] |> stack


noteBox : ConditionType -> String -> Collage Msg
noteBox conditionType label =
    let
        ( w, h ) =
            ( unit * 14, unit * 5 )

        title =
            conditionType
                |> Debug.toString
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
                    ( uniform (rgb 220 237 248)
                    , solid thin (uniform black)
                    )
                |> align topRight
                |> shift ( unit, unit )
                |> impose title
                |> align topLeft
                |> shift ( -unit, unit * 2.5 )
                |> impose text

        htmlText =
            multilineEditableTextBox conditionType label

        text =
            html ( w * 2 - unit * 2, h * 2 - unit * 3.5 ) htmlText
                |> align topLeft
    in
    shape
        |> name (Debug.toString conditionType)


addConditions : Model -> Collage Msg -> Collage Msg
addConditions model tree =
    let
        correctionCoordinates name =
            case locate name base tree of
                Just ( x, y ) ->
                    ( -x, -y )

                Nothing ->
                    Debug.log ("Coordinate not found " ++ name) ( 0, 0 )
    in
    tree
        |> shift ( -19 * unit, 0 )
        |> shift (correctionCoordinates "Start")
        |> stackTwo
            (noteBox Precondition model.precondition
                |> align left
            )
        |> connect [ ( "Start", right ), ( "Precondition", left ) ] (dash verythin (uniform black))
        |> shift (correctionCoordinates "End")
        |> stackTwo
            (noteBox Postcondition model.postcondition
                |> align bottomLeft
                |> shift ( 0, -3 * unit )
            )
        |> connect [ ( "End", right ), ( "Postcondition", left ) ] (dash verythin (uniform black))



{--

  Put everything together

--}


completeTree : Model -> Collage Msg
completeTree model =
         drawTree model model.tree
            |> at left gap
            |> at right gap


treeWithConditions : Model -> List (Html.Attribute Msg) -> Html Msg
treeWithConditions model msgAttributeHtmlList =
    div msgAttributeHtmlList
      [ completeTree model
                  |> addConditions model
                  |> svg

                    --, text ("Debug info, model.tree: " ++ toStringRec model.tree)
                  ]

offsetLeft : Model -> Float
offsetLeft model =
    let
        envL =
            envelope Left (completeTree model)
    in
    max (10 + 253 + 61 - envL) 0
