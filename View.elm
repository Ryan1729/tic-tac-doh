module View exposing (view)

import Model exposing (Model, Size(..), Stack(..), Board(..))
import Html exposing (Html, text)
import Msg exposing (Msg(..))
import Material.Button as Button
import Svg exposing (Svg, svg, polygon, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)


view : Model -> Html Msg
view model =
    Html.div []
        [ model
            |> toString
            |> text
        , Button.render Mdl
            [ 0 ]
            model.mdl
            [ Button.raised
            , Button.ripple
            , Button.onClick NoOp
            ]
            [ text "test Button" ]
        , svg [ width "600", height "600", viewBox "0 0 600 600" ]
            [ Model.threeByThree (Single Pawn)
                (Single Drone)
                (Single Queen)
                (Single Pawn)
                (Single Drone)
                (Single Queen)
                FullTree
                EmptyStack
                FullNest
                |> renderBoard model.selected
            ]
        ]


boardWidth =
    600


boardHeight =
    600


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
                , pyramid Drone x (above Drone y)
                , pyramid Pawn x (above Pawn (above Drone y))
                ]

        PartialTree ->
            g []
                [ pyramid Queen x y
                , pyramid Drone x (above Drone y)
                ]

        DroneTree ->
            g []
                [ pyramid Drone x y
                , pyramid Pawn x (above Pawn y)
                ]

        NoDroneTree ->
            g []
                [ pyramid Queen x y
                , --special case
                  pyramid Pawn x (above Pawn (above Pawn y))
                ]

        FullNest ->
            g []
                [ pyramid Pawn x (below Drone (below Queen y))
                , pyramid Drone x (below Queen y)
                , pyramid Queen x y
                ]

        PartialNest ->
            g []
                [ pyramid Pawn x (below Drone (below Queen y))
                , pyramid Drone x (below Queen y)
                ]

        DroneNest ->
            g []
                [ pyramid Drone x (below Queen y)
                , pyramid Queen x y
                ]

        NoDroneNest ->
            g []
                [ --special case
                  pyramid Pawn x (below Queen (below Queen y))
                , pyramid Queen x y
                ]



--size of thing placed above this


below size y =
    y - (getScale size * 1 / 5)



--size of thing being placed above


above size y =
    y - (getScale size * 7 / 5)


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
            ++ (" l 0 " ++ toString (-3 * scale))



-- ++ (" l 0 " ++ toString (3 * scale))


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
