module Main exposing (..)

{--

  Main, calls upon the according /Tree/ files and Save to run the flowchartmaker

--}

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Save exposing (..)
import Tree.Draw as Draw exposing (..)
import Tree.State as State exposing (..)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { tree : State.Model
    , jsonDebugView : Maybe (Html State.Msg)
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tree = State.init
      , jsonDebugView = Nothing
      }
    , Cmd.none
    )


type Msg
    = Tree State.Msg
    | Save Save.Msg
    -- | GenerateJsonDebug


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tree treeMsg ->
            let
                ( treeModel, treeCmd ) =
                    State.update treeMsg model.tree
            in
            ( { model | tree = treeModel }
            , Cmd.map Tree treeCmd
            )

        Save saveMsg ->
            let
                ( saveModel, saveCmd ) =
                    Save.update saveMsg model.tree
            in
            ( { model | tree = saveModel }
            , Cmd.map Save saveCmd
            )

        -- GenerateJsonDebug ->
        --     ( { model | jsonDebugView = Just (Save.debug model.tree) }
        --     , Cmd.none
        --     )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Save (Save.subscriptions model.tree)


view : Model -> Html Msg
view model =
    let
        saveLayout =
            style "position" "absolute"--[ ( "position", "absolute" ), ( "height", "100%" ), ( "top", "25px" ), ( "left", "10px" ), ( "z-index", "10" ) ]

        treeLayout =
            style "position" "absolute" --[ ( "position", "absolute" ), ( "top", "25px" ), ( "height", "100%" ), ( "left", Debug.toString (offsetLeft model.tree) ++ "px" ), ( "z-index", "0" ) ]
    in
    div [ style "overflow-y" "auto" ] --), ( "height", "100%" ) ] ]
        [ div
            [ style "position" "absolute"]--[ ( "position", "absolute" ), ( "margin-right", "10px" ), ( "top", "20px" ), ( "background-color", "#e62272" ), ( "z-index", "-1" ) ] ]
            [ Html.map Save
                (Save.view model.tree [ saveLayout ])
            , Html.map Tree
                (Draw.treeWithConditions model.tree [ treeLayout ])
            ]

        --, jsonDebug model
        ]


-- jsonDebug : Model -> Html Msg
-- jsonDebug model =
--     let
--         unpackedView =
--             case model.jsonDebugView of
--                 Just view ->
--                     view
--
--                 Nothing ->
--                     div [] []
--     in
--     div
--         [ style [ ( "top", "10px" ) ] ]
--         [ button [ onClick GenerateJsonDebug ] [ text "Generate Json" ]
--         , br [] []
--         , Html.map Tree unpackedView
--         ]
