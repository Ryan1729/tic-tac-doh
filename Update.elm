module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, Board, Size(..), Outcome(..), BoardId, EdgeId)
import Material
import Extras
import Random.Pcg as Random exposing (Seed)


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
            if model.outcome == TBD && Model.stashGet size model.stash >= 1 then
                let
                    _ =
                        Debug.log "beforeboard" model.board

                    postPlacementModel =
                        applyPlaceFunction placeFunction size model

                    a_ =
                        Debug.log "afterboard" postPlacementModel.board

                    newModel =
                        { postPlacementModel
                            | player = Model.CPU
                        }

                    postCPUTurnModel =
                        case cpuTurn newModel of
                            Just postCPUModel ->
                                postCPUModel

                            Nothing ->
                                { newModel
                                    | outcome =
                                        case newModel.outcome of
                                            TBD ->
                                                UserWin

                                            otherOutcome ->
                                                otherOutcome
                                }
                in
                    ( { postCPUTurnModel | player = Model.User }
                    , Cmd.none
                    )
            else
                ( { model | selected = Nothing }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


applyPlaceFunction : (Size -> Board -> Board) -> Size -> Model -> Model
applyPlaceFunction placeFunction size model =
    let
        newBoard =
            placeFunction size model.board

        newStashAmount =
            (Model.stashGet size model.stash) - 1

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
        { newModel | outcome = Model.getOutcome newModel }


applyMove : Model -> Move -> Model
applyMove model move =
    case move of
        BoardMove ( boardId, size ) ->
            applyPlaceFunction (Model.place boardId) size model

        EdgeMove ( edgeId, size ) ->
            applyPlaceFunction (Model.placeOnEdge edgeId) size model


type Move
    = BoardMove ( BoardId, Size )
    | EdgeMove ( EdgeId, Size )


cpuTurn : Model -> Maybe Model
cpuTurn model =
    if model.outcome == TBD && model.player == Model.CPU then
        let
            _ =
                Debug.log "cpu sees this board" model.board

            availableBoardIdSizePairs : List ( BoardId, Size )
            availableBoardIdSizePairs =
                Model.getAvailableBoardIdSizePairs model.board model.stash

            availableEdgeIdSizePairs : List ( EdgeId, Size )
            availableEdgeIdSizePairs =
                Model.getAvailableEdgeIdSizePairs model.board model.stash

            moves : List Move
            moves =
                (List.map BoardMove availableBoardIdSizePairs
                    ++ List.map EdgeMove availableEdgeIdSizePairs
                )
                    |> shuffle (Random.initialSeed 42)

            maybeWinningMove =
                Extras.find (applyMove model >> Model.getOutcome >> (==) Model.CPUWin) moves
        in
            case maybeWinningMove of
                Just move ->
                    move
                        |> Debug.log "Winner"
                        |> applyMove model
                        |> Just

                Nothing ->
                    let
                        maybeMove =
                            Random.step (Random.sample moves) (Random.initialSeed 42)
                                |> fst
                                |> Debug.log ""
                    in
                        Maybe.map (applyMove model) maybeMove
    else
        Nothing
            |> Debug.log "?"


shuffle : Seed -> List a -> List a
shuffle seed list =
    let
        length =
            List.length list

        randomTags =
            Random.step (Random.list length (Random.int 0 length)) seed
                |> fst
    in
        List.map2 (,) randomTags list |> List.sortBy fst |> List.unzip |> snd
