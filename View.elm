module View exposing (view)

import Model exposing (Model, Size(..), Stack(..), Board(..), Stash)
import Html exposing (Html, text)
import Html.Attributes
import Msg exposing (Msg(..))
import Material.Button as Button
import Material.Grid as Grid exposing (Device(..))
import Svg exposing (Svg, svg, polygon, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


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
                [ svg
                    [ width stashWidthString
                    , height stashHeightString
                    , viewBox ("0 0 " ++ stashWidthString ++ " " ++ stashHeightString)
                    ]
                    [ renderStash model.stash
                    ]
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


stashWidth =
    boardWidth


stashHeight =
    above Queen Queen 0
        |> abs
        |> (*) (Model.maxStashAmount - 1)
        |> (+) (pyramidHeightConstant * getScale Queen)
        |> (+) (2 * stashSpacing)


stashWidthString =
    toString stashWidth


stashHeightString =
    toString stashHeight


renderStash : Stash -> Svg Msg
renderStash stash =
    g []
        <| [ Svg.rect
                [ x "0"
                , y "0"
                , width boardWidthString
                , height stashHeightString
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                []
           ]
        ++ stashStack (stashWidth * 1 / 6) stash.queen Queen
        ++ stashStack (stashWidth * 3 / 6) stash.drone Drone
        ++ stashStack (stashWidth * 5 / 6) stash.pawn Pawn


stashStack : Float -> Int -> Size -> List (Svg Msg)
stashStack x amount size =
    getHeights amount size
        |> List.reverse
        |> List.tail
        |> Maybe.withDefault []
        |> List.reverse
        |> List.map (pyramid size x)


getHeights : Int -> Size -> List Float
getHeights amount size =
    List.repeat amount size
        |> (List.scanl (above size) (stashHeight - stashSpacing))


queenStashWidth =
    stashSpacing + pyramidHeightConstant * getScale Queen


droneStashWidth =
    queenStashWidth + stashSpacing + 2 * getScale Drone


pawnStashWidth =
    droneStashWidth + stashSpacing + 2 * getScale Pawn


stashSpacing =
    5


renderStashPiece : Float -> Int -> Size -> Svg Msg
renderStashPiece x index size =
    let
        y =
            (toFloat index * stashWidth / 5) + (getScale Queen + stashSpacing)
    in
        pyramid size x y


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


nullSvg =
    Svg.polygon [] []


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


renderStack : Stack -> Float -> Float -> Svg Msg
renderStack stack x y =
    case stack of
        EmptyStack ->
            nullSvg

        Single size ->
            pyramid size x y

        FullTree ->
            g []
                [ pyramid Queen x y
                , pyramid Drone x (above Drone Queen y)
                , pyramid Pawn x (above Pawn Drone (above Drone Queen y))
                ]

        PartialTree ->
            g []
                [ pyramid Queen x y
                , pyramid Drone x (above Drone Queen y)
                ]

        DroneTree ->
            g []
                [ pyramid Drone x y
                , pyramid Pawn x (above Pawn Drone y)
                ]

        NoDroneTree ->
            g []
                [ pyramid Queen x y
                , pyramid Pawn x (above Pawn Queen y)
                ]

        FullNest ->
            g []
                [ pyramid Pawn x (below Pawn Drone (below Drone Queen y))
                , pyramid Drone x (below Drone Queen y)
                , pyramid Queen x y
                ]

        PartialNest ->
            g []
                [ pyramid Pawn x (below Pawn Drone y)
                , pyramid Drone x y
                ]

        DroneNest ->
            g []
                [ pyramid Drone x (below Drone Queen y)
                , pyramid Queen x y
                ]

        NoDroneNest ->
            g []
                [ --special case
                  pyramid Pawn x (below Pawn Queen y)
                , pyramid Queen x y
                ]


below lowerSize upperSize y =
    if lowerSize == upperSize then
        y
    else
        case ( lowerSize, upperSize ) of
            ( Pawn, Queen ) ->
                y - (getScale upperSize * 2 / 5)

            _ ->
                y - (getScale upperSize * 1 / 5)


above upperSize lowerSize y =
    if lowerSize == upperSize then
        y - (getScale upperSize * 4 / 5)
    else
        case ( upperSize, lowerSize ) of
            ( Pawn, Queen ) ->
                y - (getScale Pawn * 14 / 5)

            _ ->
                y - (getScale upperSize * 7 / 5)


pawnPyramidPathSuffix =
    Pawn
        |> getScale
        |> pyramidPathSuffix


dronePyramidPathSuffix =
    Drone
        |> getScale
        |> pyramidPathSuffix


queenPyramidPathSuffix =
    Queen
        |> getScale
        |> pyramidPathSuffix


getScale : Size -> Float
getScale size =
    case size of
        Queen ->
            (160 / 3)

        Drone ->
            40

        Pawn ->
            30


dotAttributes dotScale =
    [ stroke "#EEEEEE"
    , fillOpacity "0.0"
    , dotScale
        / 9
        |> toString
        |> rx
    , dotScale
        / 7
        |> toString
        |> ry
    ]


middleY =
    9 / 10


queen2ndX =
    21 / 32


queen2ndY =
    17 / 16


queen3rdX =
    26 / 32


queen3rdY =
    19 / 16


drone2ndX =
    3 / 4


drone2ndY =
    11 / 10


getDots : Size -> Float -> Float -> List (Svg Msg)
getDots size x y =
    let
        scale =
            getScale size

        pawnScale =
            getScale Pawn

        leftMiddleDot =
            dot pawnScale
                [ (x + (-scale / 2))
                    |> toString
                    |> cx
                , (y + -(scale * middleY))
                    |> toString
                    |> cy
                ]

        rightMiddleDot =
            dot pawnScale
                [ (x + (scale / 2))
                    |> toString
                    |> cx
                , (y + -(scale * middleY))
                    |> toString
                    |> cy
                ]
    in
        case size of
            Queen ->
                [ leftMiddleDot
                , dot (pawnScale * 31 / 32)
                    [ (x + (-scale * queen2ndX))
                        |> toString
                        |> cx
                    , (y - (scale * queen2ndY))
                        |> toString
                        |> cy
                    ]
                , dot (pawnScale * 30 / 32)
                    [ (x + (-scale * queen3rdX))
                        |> toString
                        |> cx
                    , (y - (scale * queen3rdY))
                        |> toString
                        |> cy
                    ]
                , rightMiddleDot
                , dot (pawnScale * 33 / 32)
                    [ (x + (scale * (1 - queen2ndX)))
                        |> toString
                        |> cx
                    , (y - (scale * ((2 * middleY) - (queen2ndY))))
                        |> toString
                        |> cy
                    ]
                , dot (pawnScale * 34 / 32)
                    [ (x + (scale * (1 - queen3rdX)))
                        |> toString
                        |> cx
                    , (y - (scale * ((2 * middleY) - (queen3rdY))))
                        |> toString
                        |> cy
                    ]
                ]

            Drone ->
                [ leftMiddleDot
                , dot (pawnScale * 31 / 32)
                    [ (x + (-scale * drone2ndX))
                        |> toString
                        |> cx
                    , (y - (scale * drone2ndY))
                        |> toString
                        |> cy
                    ]
                , rightMiddleDot
                , dot (pawnScale * 33 / 32)
                    [ (x + (scale * (1 - drone2ndX)))
                        |> toString
                        |> cx
                    , (y - (scale * ((2 * middleY) - drone2ndY)))
                        |> toString
                        |> cy
                    ]
                ]

            Pawn ->
                [ leftMiddleDot
                , rightMiddleDot
                ]


dot dotScale attributes =
    ellipse
        (attributes
            ++ dotAttributes dotScale
        )
        []


pyramidPathSuffix : Float -> String
pyramidPathSuffix scale =
    let
        sclaeString =
            toString scale

        doubleSclaeString =
            toString (2 * scale)
    in
        (" l " ++ sclaeString ++ " -" ++ sclaeString)
            ++ ("l -" ++ sclaeString ++ " -" ++ doubleSclaeString)
            ++ (" l -" ++ sclaeString ++ " " ++ doubleSclaeString)
            ++ (" l " ++ sclaeString ++ " " ++ sclaeString)
            ++ " Z "
            ++ (" l 0 " ++ toString (-pyramidHeightConstant * scale))


pyramidHeightConstant =
    3


pyramid : Size -> Float -> Float -> Svg Msg
pyramid size x y =
    let
        dString =
            "M "
                ++ toString x
                ++ " "
                ++ toString y
                ++ case size of
                    Queen ->
                        queenPyramidPathSuffix

                    Drone ->
                        dronePyramidPathSuffix

                    Pawn ->
                        pawnPyramidPathSuffix
    in
        [ Svg.path
            [ d dString
            , stroke "grey"
            , strokeWidth "2"
            , fillOpacity "0.4"
            ]
            []
        ]
            ++ getDots size x y
            |> g []
