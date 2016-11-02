module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, Board, Size, Outcome(..))
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
                if model.outcome == TBD && stashAmount >= 1 then
                    let
                        newBoard =
                            placeFunction size model.board

                        newStashAmount =
                            stashAmount - 1

                        newModel =
                            { model
                                | board = newBoard
                                , stash = Model.stashSet size newStashAmount model.stash
                                , selected =
                                    if newStashAmount >= 1 then
                                        model.selected
                                    else
                                        Nothing
                            }
                    in
                        ( { newModel
                            | outcome = Model.getOutcome newModel
                            , player = Model.nextPlayer newModel.player
                          }
                        , Cmd.none
                        )
                else
                    ( { model | selected = Nothing }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )
