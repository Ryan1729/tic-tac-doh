module View exposing (view)

import Model exposing (Model, Size(..), Stack(..), Board(..), Stash)
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
    case board of
        EmptyBoard ->
            g []
                <| spaceAndStack selected EmptyStack atCenter

        OneByOne stack ->
            g []
                <| spaceAndStack selected stack atCenter

        OneByTwo stack0 stack1 ->
            g []
                <| spaceAndStack selected stack0 (fromCenter -halfSpaceOffset -halfSpaceOffset)
                ++ spaceAndStack selected stack1 (fromCenter halfSpaceOffset halfSpaceOffset)

        OneByThree stack0 stack1 stack2 ->
            g []
                <| spaceAndStack selected stack0 (fromCenter -spaceOffset -spaceOffset)
                ++ spaceAndStack selected stack1 atCenter
                ++ spaceAndStack selected stack2 (fromCenter spaceOffset spaceOffset)

        TwoByTwo spaces ->
            g []
                <| spaceAndStack selected spaces.zeroZero atCenter
                ++ spaceAndStack selected spaces.zeroOne (fromCenter spaceOffset spaceOffset)
                ++ spaceAndStack selected spaces.oneZero (fromCenter -spaceOffset spaceOffset)
                ++ spaceAndStack selected spaces.oneOne (fromCenter 0 doubleSpaceOffset)

        TwoByThree spaces ->
            g []
                <| spaceAndStack selected spaces.zeroZero (fromCenter -halfSpaceOffset -halfSpaceOffset)
                ++ spaceAndStack selected spaces.zeroOne (fromCenter halfSpaceOffset halfSpaceOffset)
                ++ spaceAndStack selected spaces.zeroTwo (fromCenter threeHalfsSpaceOffset threeHalfsSpaceOffset)
                ++ spaceAndStack selected spaces.oneZero (fromCenter -threeHalfsSpaceOffset halfSpaceOffset)
                ++ spaceAndStack selected spaces.oneOne (fromCenter -halfSpaceOffset threeHalfsSpaceOffset)
                ++ spaceAndStack selected spaces.oneTwo (fromCenter halfSpaceOffset (2.5 * spaceOffset))

        ThreeByThree spaces ->
            g []
                <| spaceAndStack selected spaces.zeroZero (fromCenter 0 -spaceOffset)
                ++ spaceAndStack selected spaces.zeroOne (fromCenter spaceOffset 0)
                ++ spaceAndStack selected spaces.zeroTwo (fromCenter doubleSpaceOffset spaceOffset)
                ++ spaceAndStack selected spaces.oneZero (fromCenter -spaceOffset 0)
                ++ spaceAndStack selected spaces.oneOne (fromCenter 0 spaceOffset)
                ++ spaceAndStack selected spaces.oneTwo (fromCenter spaceOffset doubleSpaceOffset)
                ++ spaceAndStack selected spaces.twoZero (fromCenter -doubleSpaceOffset spaceOffset)
                ++ spaceAndStack selected spaces.twoOne (fromCenter -spaceOffset doubleSpaceOffset)
                ++ spaceAndStack selected spaces.twoTwo (fromCenter 0 (3 * spaceOffset))


atCenter =
    ( centerX, centerY )


fromCenter x y =
    ( centerX + x, centerY + y )


tupleAdd ( x, y ) ( xOffset, yOffset ) =
    ( x + xOffset, y + yOffset )


spaceAndStack : Maybe Size -> Stack -> ( Float, Float ) -> List (Svg Msg)
spaceAndStack selected stack ( x, y ) =
    [ spaceMsg selected stack
        |> space x y
    , renderStack stack x y
    ]


spaceMsg : Maybe Size -> Stack -> Maybe Msg
spaceMsg selected stack =
    case selected of
        Just size ->
            if sizeFits size stack then
                Just Place
            else
                Nothing

        Nothing ->
            Nothing


sizeFits : Size -> Stack -> Bool
sizeFits size stack =
    case stack of
        EmptyStack ->
            True

        Single stackSize ->
            size /= stackSize

        PartialTree ->
            size == Pawn

        PartialNest ->
            size == Queen

        FullTree ->
            False

        DroneTree ->
            False

        NoDroneTree ->
            False

        DroneNest ->
            False

        FullNest ->
            False

        NoDroneNest ->
            False


space : Float -> Float -> Maybe Msg -> Svg Msg
space x y maybeMsg =
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
