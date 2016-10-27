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
            [ renderBoard 140 400 EmptyBoard
            , (Single Pawn)
                |> OneByOne
                |> renderBoard 140 200
            , (Single Drone)
                |> OneByOne
                |> renderBoard 300 200
            , (Single Queen)
                |> OneByOne
                |> renderBoard 460 200
              -- , TwoByTwo
              --     { zeroZero : Maybe Stack
              --     , zeroOne : Maybe Stack
              --     , oneZero : Maybe Stack
              --     , oneOne : Maybe Stack
              --     }
              -- , ThreeByThree
              --     { zeroZero : Maybe Stack
              --     , zeroOne : Maybe Stack
              --     , zeroTwo : Maybe Stack
              --     , oneZero : Maybe Stack
              --     , oneOne : Maybe Stack
              --     , oneTwo : Maybe Stack
              --     , twoOne : Maybe Stack
              --     , twoZero : Maybe Stack
              --     , twoTwo : Maybe Stack
              --     }
            ]
        ]


renderBoard : Float -> Float -> Board -> Svg Msg
renderBoard x y board =
    case board of
        EmptyBoard ->
            space x y

        OneByOne stack ->
            g []
                [ space x y
                , renderStack stack x y
                ]

        _ ->
            space x y


square x =
    x ^ 2


space : Float -> Float -> Svg Msg
space x y =
    let
        queenScale =
            getScale Queen

        queenScaleString =
            toString queenScale

        spaceSideLength =
            sqrt (2.5 * square queenScale) * 7 / 5

        spaceOffset =
            sqrt (square spaceSideLength / 2)

        spaceOffsetString =
            toString spaceOffset

        minusSpaceOffsetString =
            toString -spaceOffset

        dString =
            ("M " ++ toString x ++ " " ++ toString y)
                ++ (" m 0 " ++ toString (queenScale / 2))
                ++ (" l " ++ spaceOffsetString ++ " " ++ minusSpaceOffsetString)
                ++ (" l " ++ minusSpaceOffsetString ++ " " ++ minusSpaceOffsetString)
                ++ (" l " ++ minusSpaceOffsetString ++ " " ++ spaceOffsetString)
                ++ (" l " ++ spaceOffsetString ++ " " ++ spaceOffsetString)
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
