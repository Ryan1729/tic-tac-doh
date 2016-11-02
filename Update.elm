module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model, Board, Size, Outcome(..))
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
                    postPlacementModel =
                        applyPlaceFunction placeFunction size model

                    newModel =
                        { postPlacementModel
                            | outcome = Model.getOutcome postPlacementModel
                            , player = Model.nextPlayer postPlacementModel.player
                        }

                    finalModel =
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
                    ( finalModel
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
    in
        { model
            | board = newBoard
            , stash = Model.stashSet size newStashAmount model.stash
            , selected =
                if newStashAmount >= 1 then
                    model.selected
                else
                    Nothing
        }


cpuTurn : Model -> Maybe Model
cpuTurn model =
    if model.outcome == TBD && model.player == Model.CPU then
        let
            placeFunctions : List (Model -> Model)
            placeFunctions =
                --TODO applyPlaceFunction [placeFunctions]
                []
                    |> shuffle (Random.initialSeed 42)

            maybeWinningMove =
                Extras.find (\f -> model |> f |> Model.getOutcome |> (==) Model.CPUWin) placeFunctions
        in
            case maybeWinningMove of
                Just move ->
                    model
                        |> move
                        |> Just

                Nothing ->
                    let
                        maybeMove =
                            Random.step (Random.sample placeFunctions) (Random.initialSeed 42)
                                |> fst
                    in
                        Maybe.map (\move -> move model) maybeMove
    else
        Nothing


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
