module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Select selected ->
            ( { model | selected = selected }, Cmd.none )

        Place boardId ->
            case model.selected of
                Just size ->
                    let
                        stashAmount =
                            Model.stashGet size model.stash
                    in
                        if stashAmount >= 1 then
                            ( { model
                                | board = Model.place boardId size model.board
                                , stash = Model.stashSet size (stashAmount - 1) model.stash
                              }
                            , Cmd.none
                            )
                        else
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Mdl msg' ->
            Material.update msg' model
