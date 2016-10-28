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
            [ Model.threeByThree (Just <| Single Pawn)
                (Just <| Single Drone)
                (Just <| Single Queen)
                (Just <| Single Pawn)
                (Just <| Single Drone)
                (Just <| Single Queen)
                (Just <| FullTree)
                (Just <| EmptyStack)
                (Just <| FullNest)
                |> renderBoard
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


renderBoard : Board -> Svg Msg
renderBoard board =
    case board of
        EmptyBoard ->
            space centerX centerY

        OneByOne stack ->
            g []
                <| spaceAndStack (Just stack) atCenter

        OneByTwo stack0 stack1 ->
            g []
                <| spaceAndStack stack0 (fromCenter -halfSpaceOffset -halfSpaceOffset)
                ++ spaceAndStack stack1 (fromCenter halfSpaceOffset halfSpaceOffset)

        OneByThree stack0 stack1 stack2 ->
            g []
                <| spaceAndStack stack0 (fromCenter -spaceOffset -spaceOffset)
                ++ spaceAndStack stack1 atCenter
                ++ spaceAndStack stack2 (fromCenter spaceOffset spaceOffset)

        TwoByTwo spaces ->
            g []
                <| spaceAndStack spaces.zeroZero atCenter
                ++ spaceAndStack spaces.zeroOne (fromCenter spaceOffset spaceOffset)
                ++ spaceAndStack spaces.oneZero (fromCenter -spaceOffset spaceOffset)
                ++ spaceAndStack spaces.oneOne (fromCenter 0 doubleSpaceOffset)

        TwoByThree spaces ->
            g []
                <| spaceAndStack spaces.zeroZero (fromCenter -halfSpaceOffset -halfSpaceOffset)
                ++ spaceAndStack spaces.zeroOne (fromCenter halfSpaceOffset halfSpaceOffset)
                ++ spaceAndStack spaces.zeroTwo (fromCenter threeHalfsSpaceOffset threeHalfsSpaceOffset)
                ++ spaceAndStack spaces.oneZero (fromCenter -threeHalfsSpaceOffset halfSpaceOffset)
                ++ spaceAndStack spaces.oneOne (fromCenter -halfSpaceOffset threeHalfsSpaceOffset)
                ++ spaceAndStack spaces.oneTwo (fromCenter halfSpaceOffset (2.5 * spaceOffset))

        ThreeByThree spaces ->
            g []
                <| spaceAndStack spaces.zeroZero (fromCenter 0 -spaceOffset)
                ++ spaceAndStack spaces.zeroOne (fromCenter spaceOffset 0)
                ++ spaceAndStack spaces.zeroTwo (fromCenter doubleSpaceOffset spaceOffset)
                ++ spaceAndStack spaces.oneZero (fromCenter -spaceOffset 0)
                ++ spaceAndStack spaces.oneOne (fromCenter 0 spaceOffset)
                ++ spaceAndStack spaces.oneTwo (fromCenter spaceOffset doubleSpaceOffset)
                ++ spaceAndStack spaces.twoZero (fromCenter -doubleSpaceOffset spaceOffset)
                ++ spaceAndStack spaces.twoOne (fromCenter -spaceOffset doubleSpaceOffset)
                ++ spaceAndStack spaces.twoTwo (fromCenter 0 (3 * spaceOffset))


atCenter =
    ( centerX, centerY )


fromCenter x y =
    ( centerX + x, centerY + y )


tupleAdd ( x, y ) ( xOffset, yOffset ) =
    ( x + xOffset, y + yOffset )


type alias StackDisplayer =
    --TODO: Stack -> ( Float, Float ) -> List (Svg Msg)
    Maybe Stack -> ( Float, Float ) -> List (Svg Msg)


spaceAndStack : StackDisplayer
spaceAndStack maybeStack ( x, y ) =
    case maybeStack of
        Just stack ->
            [ space x y
            , renderStack stack x y
            ]

        Nothing ->
            [ space x y ]


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


spaceOffsetString =
    toString spaceOffset


threeHalfsSpaceOffset =
    halfSpaceOffset * 3


doubleSpaceOffset =
    spaceOffset * 2


minusSpaceOffsetString =
    toString -spaceOffset


spaceSuffix =
    (" m 0 " ++ toString (queenScale / 2))
        ++ (" l " ++ spaceOffsetString ++ " " ++ minusSpaceOffsetString)
        ++ (" l " ++ minusSpaceOffsetString ++ " " ++ minusSpaceOffsetString)
        ++ (" l " ++ minusSpaceOffsetString ++ " " ++ spaceOffsetString)
        ++ (" l " ++ spaceOffsetString ++ " " ++ spaceOffsetString)


space : Float -> Float -> Svg Msg
space x y =
    let
        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ spaceSuffix
    in
        g []
            [ Svg.path
                [ d dString
                , stroke "black"
                , fill "#444444"
                , strokeWidth "4"
                ]
                []
            ]


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
            ++ (" l 0 " ++ toString (-3 * scale))


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
