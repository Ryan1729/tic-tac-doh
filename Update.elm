module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, Board, Size)
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewGame ->
            ( Model.defaultState, Cmd.none )

        Select selected ->
            ( { model | selected = selected }, Cmd.none )

        Place boardId ->
            placeMap (Model.place boardId) model

        PlaceOnEdge edgeId ->
            placeMap (Model.placeOnEdge edgeId) model

        Mdl msg' ->
            Material.update msg' model


placeMap : (Size -> Board -> Board) -> Model -> ( Model, Cmd Msg )
placeMap placeFunction model =
    case model.selected of
        Just size ->
            let
                stashAmount =
                    Model.stashGet size model.stash
            in
                if stashAmount >= 1 then
                    ( { model
                        | board = placeFunction size model.board
                        , stash = Model.stashSet size (stashAmount - 1) model.stash
                        , selected =
                            if stashAmount >= 2 then
                                model.selected
                            else
                                Nothing
                      }
                    , Cmd.none
                    )
                else
                    ( model, Cmd.none )

        Nothing ->
            ( model, Cmd.none )
