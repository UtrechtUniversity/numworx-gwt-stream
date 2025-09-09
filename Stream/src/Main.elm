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


main : Program String Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { state : State.Model
    }


init : String -> ( Model, Cmd Msg )
init model =
    let
        initModel = 
            case State.fromJson model of
               Just newModel ->
                   newModel

               Nothing ->
                   State.init
    in
    ( { state = initModel
      }
    , Cmd.none
    )


type Msg
    = Tree State.Msg
    | Save Save.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tree treeMsg ->
            let
                ( treeModel, treeCmd ) =
                    State.update treeMsg model.state
            in
            ( { model | state = treeModel }
            , Cmd.map Tree treeCmd
            )

        Save saveMsg ->
            let
                ( saveModel, saveCmd ) =
                    Save.update saveMsg model.state
            in
            ( { model | state = saveModel }
            , Cmd.map Save saveCmd
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
view model =
    let
        treeLayout =
            css
                [ position relative
                , paddingBottom (px 0)
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
            [ 
              map Tree
                (Draw.treeWithConditions model.state [ treeLayout ])
            ]

        -- , jsonDebug model
        ]
        |> toUnstyled



-- Show a encoded and then decoded model at the right of the original flowchart
-- For debugging purposes only


jsonDebug : Model -> Html Msg
jsonDebug model =
    let
        debugTreeLayout =
            css [ position absolute, right (px 0) ]
    in
    div
        [ css [ top (px 10) ] ]
        [ map Tree
            (Save.debug model.state [ debugTreeLayout ])
        ]
