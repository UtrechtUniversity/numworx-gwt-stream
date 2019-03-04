module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

{--

  Main, calls upon the according /Tree/ files and Save to run the flowchartmaker

--}

import Browser exposing (element)
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
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


view : Model -> Html.Html Msg
view model =
    let
        treeLayout =
            css
                [ position relative
                , paddingBottom (px 50)
                , zIndex (int 0)
                ]
    in
    div
        [ css [ overflowY auto ]
        ]
        [ div
            [ css
                [ position absolute
                , zIndex (int -1)
                ]
            ]
            [ map Save
                (Save.view model.tree)
            , map Tree
                (Draw.treeWithConditions model.tree [ treeLayout ])
            ]

        --, jsonDebug model
        ]
        |> toUnstyled



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
