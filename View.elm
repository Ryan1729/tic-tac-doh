module View exposing (view)

import Model exposing (Model, Size(..), Stack(..), Board(..), BoardId(..), EdgeId(..), Stash)
import Html exposing (Html, text)
import Html.Attributes
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, polygon, Attribute, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import PyramidView exposing (renderStash, renderStack, getScale)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Html.Attributes.style [ ( "display", "flex" ), ( "justify-content", "center" ) ] ]
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.ripple
                , Button.onClick NoOp
                ]
                [ text "New Game" ]
            ]
        , Grid.grid []
            [ Grid.cell [ Grid.size All 6 ]
                [ renderStash model.selected model.stash
                ]
            , Grid.cell [ Grid.size All 6 ]
                [ svg
                    [ width boardWidthString
                    , height boardHeightString
                    , viewBox ("0 0 " ++ boardWidthString ++ " " ++ boardHeightString)
                    ]
                    [ renderBoard model.selected model.board
                    ]
                ]
            ]
        ]


boardWidth =
    600


boardWidthString =
    toString boardWidth


boardHeight =
    600


boardHeightString =
    toString boardHeight


centerX =
    boardWidth / 2


centerY =
    boardHeight / 2


renderBoard : Maybe Size -> Board -> Svg Msg
renderBoard selected board =
    let
        determinedSpace =
            case Debug.log "renderBoard" board of
                EmptyBoard ->
                    spaceAndStack ZeroZero selected EmptyStack (fromBoardOffset 0 spaceOffset)

                OneByOne stack ->
                    spaceAndStack ZeroZero selected stack (fromBoardOffset 0 spaceOffset)

                OneByTwo stack0 stack1 ->
                    spaceAndStack ZeroZero selected stack0 (fromBoardOffset halfSpaceOffset halfSpaceOffset)
                        ++ spaceAndStack ZeroOne selected stack1 (fromBoardOffset -halfSpaceOffset threeHalfsSpaceOffset)

                TwoByOne stack0 stack1 ->
                    spaceAndStack ZeroZero selected stack0 (fromBoardOffset -halfSpaceOffset halfSpaceOffset)
                        ++ spaceAndStack OneZero selected stack1 (fromBoardOffset halfSpaceOffset threeHalfsSpaceOffset)

                OneByThree stack0 stack1 stack2 ->
                    spaceAndStack ZeroZero selected stack0 (fromBoardOffset halfSpaceOffset 0)
                        ++ spaceAndStack OneZero selected stack1 (fromBoardOffset -halfSpaceOffset spaceOffset)
                        ++ spaceAndStack TwoZero selected stack2 (fromBoardOffset -threeHalfsSpaceOffset doubleSpaceOffset)

                ThreeByOne stack0 stack1 stack2 ->
                    spaceAndStack ZeroZero selected stack0 (fromBoardOffset -spaceOffset -spaceOffset)
                        ++ spaceAndStack ZeroOne selected stack1 atBoardOffset
                        ++ spaceAndStack ZeroTwo selected stack2 (fromBoardOffset spaceOffset spaceOffset)

                TwoByTwo spaces ->
                    spaceAndStack ZeroZero selected spaces.zeroZero atBoardOffset
                        ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset spaceOffset spaceOffset)
                        ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -spaceOffset spaceOffset)
                        ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset 0 doubleSpaceOffset)

                TwoByThree spaces ->
                    spaceAndStack ZeroZero selected spaces.zeroZero (fromBoardOffset -halfSpaceOffset -halfSpaceOffset)
                        ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset halfSpaceOffset halfSpaceOffset)
                        ++ spaceAndStack ZeroTwo selected spaces.zeroTwo (fromBoardOffset threeHalfsSpaceOffset threeHalfsSpaceOffset)
                        ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -threeHalfsSpaceOffset halfSpaceOffset)
                        ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset -halfSpaceOffset threeHalfsSpaceOffset)
                        ++ spaceAndStack OneTwo selected spaces.oneTwo (fromBoardOffset halfSpaceOffset fiveHalfsSpaceOffset)

                ThreeByThree spaces ->
                    spaceAndStack ZeroZero selected spaces.zeroZero (fromBoardOffset 0 -spaceOffset)
                        ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset spaceOffset 0)
                        ++ spaceAndStack ZeroTwo selected spaces.zeroTwo (fromBoardOffset doubleSpaceOffset spaceOffset)
                        ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -spaceOffset 0)
                        ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset 0 spaceOffset)
                        ++ spaceAndStack OneTwo selected spaces.oneTwo (fromBoardOffset spaceOffset doubleSpaceOffset)
                        ++ spaceAndStack TwoZero selected spaces.twoZero (fromBoardOffset -doubleSpaceOffset spaceOffset)
                        ++ spaceAndStack TwoOne selected spaces.twoOne (fromBoardOffset -spaceOffset doubleSpaceOffset)
                        ++ spaceAndStack TwoTwo selected spaces.twoTwo (fromBoardOffset 0 (3 * spaceOffset))
    in
        edgeSpaces selected board
            ++ determinedSpace
            |> g []


edgeSpaces : Maybe Size -> Board -> List (Svg Msg)
edgeSpaces selected board =
    case board of
        EmptyBoard ->
            []

        OneByOne _ ->
            [ edgeSpace (fromBoardOffset 0 -spaceOffset) EdgeZeroZero selected
            , edgeSpace (fromBoardOffset spaceOffset 0) EdgeOneZero selected
            , edgeSpace (fromBoardOffset doubleSpaceOffset spaceOffset) EdgeTwoZero selected
            , edgeSpace (fromBoardOffset -spaceOffset 0) EdgeZeroOne selected
            , edgeSpace (fromBoardOffset spaceOffset doubleSpaceOffset) EdgeTwoOne selected
            , edgeSpace (fromBoardOffset -doubleSpaceOffset spaceOffset) EdgeZeroTwo selected
            , edgeSpace (fromBoardOffset -spaceOffset doubleSpaceOffset) EdgeOneTwo selected
            , edgeSpace (fromBoardOffset 0 (3 * spaceOffset)) EdgeTwoTwo selected
            ]

        OneByTwo _ _ ->
            [ edgeSpace (fromBoardOffset halfSpaceOffset -threeHalfsSpaceOffset) EdgeZeroZero selected
            , edgeSpace (fromBoardOffset threeHalfsSpaceOffset -halfSpaceOffset) EdgeOneZero selected
            , edgeSpace (fromBoardOffset fiveHalfsSpaceOffset halfSpaceOffset) EdgeTwoZero selected
            , edgeSpace (fromBoardOffset -halfSpaceOffset -halfSpaceOffset) EdgeZeroOne selected
            , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset halfSpaceOffset) EdgeTwoOne selected
            , edgeSpace (fromBoardOffset threeHalfsSpaceOffset threeHalfsSpaceOffset) EdgeOneTwo selected
            , edgeSpace (fromBoardOffset halfSpaceOffset fiveHalfsSpaceOffset) EdgeTwoTwo selected
            , edgeSpace (fromBoardOffset -fiveHalfsSpaceOffset threeHalfsSpaceOffset) EdgeZeroThree selected
            , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset fiveHalfsSpaceOffset) EdgeOneThree selected
            , edgeSpace (fromBoardOffset -halfSpaceOffset (3.5 * spaceOffset)) EdgeTwoThree selected
            ]

        TwoByOne _ _ ->
            [ edgeSpace (fromBoardOffset -halfSpaceOffset -threeHalfsSpaceOffset) EdgeZeroZero selected
            , edgeSpace (fromBoardOffset halfSpaceOffset -halfSpaceOffset) EdgeOneZero selected
            , edgeSpace (fromBoardOffset threeHalfsSpaceOffset halfSpaceOffset) EdgeTwoZero selected
            , edgeSpace (fromBoardOffset fiveHalfsSpaceOffset threeHalfsSpaceOffset) EdgeThreeZero selected
            , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset -halfSpaceOffset) EdgeZeroOne selected
            , edgeSpace (fromBoardOffset threeHalfsSpaceOffset fiveHalfsSpaceOffset) EdgeThreeOne selected
            , edgeSpace (fromBoardOffset -fiveHalfsSpaceOffset halfSpaceOffset) EdgeZeroTwo selected
            , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset threeHalfsSpaceOffset) EdgeOneTwo selected
            , edgeSpace (fromBoardOffset -halfSpaceOffset fiveHalfsSpaceOffset) EdgeTwoTwo selected
            , edgeSpace (fromBoardOffset halfSpaceOffset (3.5 * spaceOffset)) EdgeThreeTwo selected
            ]

        OneByThree _ _ _ ->
            [ edgeSpace (fromBoardOffset -halfSpaceOffset -spaceOffset) EdgeZeroZero selected
            , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset 0) EdgeTwoOne selected
            , edgeSpace (fromBoardOffset threeHalfsSpaceOffset spaceOffset) EdgeTwoZero selected
            , edgeSpace (fromBoardOffset halfSpaceOffset doubleSpaceOffset) EdgeTwoTwo selected
            , edgeSpace (fromBoardOffset -fiveHalfsSpaceOffset spaceOffset) EdgeZeroThree selected
            , edgeSpace (fromBoardOffset -halfSpaceOffset (3 * spaceOffset)) EdgeTwoThree selected
            ]

        --
        -- ThreeByOne _ _ _ ->
        --     [ edgeSpace (fromBoardOffset -halfSpaceOffset -threeHalfsSpaceOffset) EdgeZeroZero selected
        --     , edgeSpace (fromBoardOffset halfSpaceOffset -halfSpaceOffset) EdgeOneZero selected
        --     , edgeSpace (fromBoardOffset threeHalfsSpaceOffset halfSpaceOffset) EdgeTwoZero selected
        --     , edgeSpace (fromBoardOffset fiveHalfsSpaceOffset threeHalfsSpaceOffset) EdgeThreeZero selected
        --     , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset -halfSpaceOffset) EdgeZeroOne selected
        --     , edgeSpace (fromBoardOffset threeHalfsSpaceOffset fiveHalfsSpaceOffset) EdgeThreeOne selected
        --     , edgeSpace (fromBoardOffset -fiveHalfsSpaceOffset halfSpaceOffset) EdgeZeroTwo selected
        --     , edgeSpace (fromBoardOffset -threeHalfsSpaceOffset threeHalfsSpaceOffset) EdgeOneTwo selected
        --     , edgeSpace (fromBoardOffset -halfSpaceOffset fiveHalfsSpaceOffset) EdgeTwoTwo selected
        --     , edgeSpace (fromBoardOffset halfSpaceOffset (3.5 * spaceOffset)) EdgeThreeTwo selected
        --     ]
        _ ->
            []



--  OneByTwo Stack Stack
-- | TwoByOne Stack Stack
-- | OneByThree Stack Stack Stack
-- | ThreeByOne Stack Stack Stack
-- OneByTwo stack0 stack1 ->
--     spaceAndStack ZeroZero selected stack0 (fromBoardOffset -halfSpaceOffset -halfSpaceOffset)
--         ++ spaceAndStack ZeroOne selected stack1 (fromBoardOffset halfSpaceOffset halfSpaceOffset)
--
-- OneByThree stack0 stack1 stack2 ->
--     spaceAndStack ZeroZero selected stack0 (fromBoardOffset -spaceOffset -spaceOffset)
--         ++ spaceAndStack ZeroOne selected stack1 atBoardOffset
--         ++ spaceAndStack ZeroTwo selected stack2 (fromBoardOffset spaceOffset spaceOffset)
--
-- TwoByTwo spaces ->
--     spaceAndStack ZeroZero selected spaces.zeroZero atBoardOffset
--         ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset spaceOffset spaceOffset)
--         ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -spaceOffset spaceOffset)
--         ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset 0 doubleSpaceOffset)
--
-- TwoByThree spaces ->
--     spaceAndStack ZeroZero selected spaces.zeroZero (fromBoardOffset -halfSpaceOffset -halfSpaceOffset)
--         ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset halfSpaceOffset halfSpaceOffset)
--         ++ spaceAndStack ZeroTwo selected spaces.zeroTwo (fromBoardOffset threeHalfsSpaceOffset threeHalfsSpaceOffset)
--         ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -threeHalfsSpaceOffset halfSpaceOffset)
--         ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset -halfSpaceOffset threeHalfsSpaceOffset)
--         ++ spaceAndStack OneTwo selected spaces.oneTwo (fromBoardOffset halfSpaceOffset (2.5 * spaceOffset))
--
-- ThreeByThree spaces ->
--     spaceAndStack ZeroZero selected spaces.zeroZero (fromBoardOffset 0 -spaceOffset)
--         ++ spaceAndStack ZeroOne selected spaces.zeroOne (fromBoardOffset spaceOffset 0)
--         ++ spaceAndStack ZeroTwo selected spaces.zeroTwo (fromBoardOffset doubleSpaceOffset spaceOffset)
--         ++ spaceAndStack OneZero selected spaces.oneZero (fromBoardOffset -spaceOffset 0)
--         ++ spaceAndStack OneOne selected spaces.oneOne (fromBoardOffset 0 spaceOffset)
--         ++ spaceAndStack OneTwo selected spaces.oneTwo (fromBoardOffset spaceOffset doubleSpaceOffset)
--         ++ spaceAndStack TwoZero selected spaces.twoZero (fromBoardOffset -doubleSpaceOffset spaceOffset)
--         ++ spaceAndStack TwoOne selected spaces.twoOne (fromBoardOffset -spaceOffset doubleSpaceOffset)
--         ++ spaceAndStack TwoTwo selected spaces.twoTwo (fromBoardOffset 0 (3 * spaceOffset))


boardOffsetX =
    centerX


boardOffsetY =
    centerY - 20


atBoardOffset =
    ( boardOffsetX, boardOffsetY )


fromBoardOffset x y =
    ( boardOffsetX + x, boardOffsetY + y )


tupleAdd ( x, y ) ( xOffset, yOffset ) =
    ( x + xOffset, y + yOffset )


spaceAndStack : BoardId -> Maybe Size -> Stack -> ( Float, Float ) -> List (Svg Msg)
spaceAndStack boardId selected stack coords =
    [ spaceMsg boardId selected stack
        |> space coords
    , renderStack stack coords
    ]


spaceMsg : BoardId -> Maybe Size -> Stack -> Maybe Msg
spaceMsg boardId selected stack =
    case selected of
        Just size ->
            if Model.sizeFits size stack then
                Just (Place boardId)
            else
                Nothing

        Nothing ->
            Nothing


space : ( Float, Float ) -> Maybe Msg -> Svg Msg
space ( x, y ) maybeMsg =
    let
        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ spaceSuffix

        msgAttributes =
            case maybeMsg of
                Just msg ->
                    [ stroke "white", onClick msg ]

                Nothing ->
                    [ stroke "black" ]
    in
        g []
            [ Svg.path
                (msgAttributes
                    ++ [ d dString
                       , fill "#444444"
                       , strokeWidth "4"
                       ]
                )
                []
            ]


edgeSpace : ( Float, Float ) -> EdgeId -> Maybe Size -> Svg Msg
edgeSpace ( x, y ) edgeId selected =
    let
        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ spaceSuffix

        msgAttributes =
            case selected of
                Just _ ->
                    [ stroke "white", onClick (PlaceOnEdge edgeId) ]

                Nothing ->
                    [ stroke "black" ]
    in
        g []
            [ Svg.path
                (msgAttributes
                    ++ [ d dString
                       , fill "#444444"
                       , fillOpacity "0.4"
                       , strokeWidth "4"
                       ]
                )
                []
            ]


square x =
    x ^ 2


queenScale =
    getScale Queen


queenScaleString =
    toString queenScale


spaceSideLength =
    sqrt (2.5 * square queenScale) * 7 / 5


spaceOffset =
    sqrt (square spaceSideLength / 2)


halfSpaceOffset =
    spaceOffset / 2


suffixSpaceOffset =
    --make room for the ourlines
    spaceOffset - 2.5


suffixSpaceOffsetString =
    toString suffixSpaceOffset


threeHalfsSpaceOffset =
    halfSpaceOffset * 3


fiveHalfsSpaceOffset =
    halfSpaceOffset * 5


doubleSpaceOffset =
    spaceOffset * 2


minusSuffixSpaceOffsetString =
    toString -suffixSpaceOffset


spaceSuffix =
    (" m 0 " ++ toString (queenScale / 2))
        ++ (" l " ++ suffixSpaceOffsetString ++ " " ++ minusSuffixSpaceOffsetString)
        ++ (" l " ++ minusSuffixSpaceOffsetString ++ " " ++ minusSuffixSpaceOffsetString)
        ++ (" l " ++ minusSuffixSpaceOffsetString ++ " " ++ suffixSpaceOffsetString)
        ++ "Z"
