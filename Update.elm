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

        Place ->
            ( model, Debug.log "TODO" Cmd.none )

        Mdl msg' ->
            Material.update msg' model
