module PyramidView exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, svg, polygon, Attribute, ellipse, g)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)
import Msg exposing (Msg(..))
import Model exposing (Model, Size(..), Stack(..), Board(..), Stash)


stashWidth =
    600


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


renderStash : Maybe Size -> Stash -> Html Msg
renderStash selected stash =
    svg
        [ width stashWidthString
        , height stashHeightString
        , viewBox ("0 0 " ++ stashWidthString ++ " " ++ stashHeightString)
        ]
        <| [ Svg.rect
                [ x "0"
                , y "0"
                , width stashWidthString
                , height stashHeightString
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                []
           ]
        ++ renderStashStack selected (stashWidth * 1 / 6) stash.queen Queen
        ++ renderStashStack selected (stashWidth * 3 / 6) stash.drone Drone
        ++ renderStashStack selected (stashWidth * 5 / 6) stash.pawn Pawn


renderStashStack : Maybe Size -> Float -> Int -> Size -> List (Svg Msg)
renderStashStack selected x amount size =
    let
        pyramidView =
            case selected of
                Just selectedSize ->
                    if size == selectedSize then
                        highlightedPyramid
                    else
                        plainPyramid

                Nothing ->
                    plainPyramid
    in
        getHeights amount size
            |> List.reverse
            |> List.tail
            |> Maybe.withDefault []
            |> List.reverse
            |> List.map (pyramidView size x)


getHeights : Int -> Size -> List Float
getHeights amount size =
    List.repeat amount size
        |> (List.scanl (above size) (stashHeight - stashSpacing))


stashSpacing =
    5


renderStack : Stack -> Float -> Float -> Svg Msg
renderStack stack x y =
    case stack of
        EmptyStack ->
            nullSvg

        Single size ->
            plainPyramid size x y

        FullTree ->
            g []
                [ plainPyramid Queen x y
                , plainPyramid Drone x (above Drone Queen y)
                , plainPyramid Pawn x (above Pawn Drone (above Drone Queen y))
                ]

        PartialTree ->
            g []
                [ plainPyramid Queen x y
                , plainPyramid Drone x (above Drone Queen y)
                ]

        DroneTree ->
            g []
                [ plainPyramid Drone x y
                , plainPyramid Pawn x (above Pawn Drone y)
                ]

        NoDroneTree ->
            g []
                [ plainPyramid Queen x y
                , plainPyramid Pawn x (above Pawn Queen y)
                ]

        FullNest ->
            g []
                [ plainPyramid Pawn x (below Pawn Drone (below Drone Queen y))
                , plainPyramid Drone x (below Drone Queen y)
                , plainPyramid Queen x y
                ]

        PartialNest ->
            g []
                [ plainPyramid Pawn x (below Pawn Drone y)
                , plainPyramid Drone x y
                ]

        DroneNest ->
            g []
                [ plainPyramid Drone x (below Drone Queen y)
                , plainPyramid Queen x y
                ]

        NoDroneNest ->
            g []
                [ plainPyramid Pawn x (below Pawn Queen y)
                , plainPyramid Queen x y
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


highlightedPyramid =
    pyramid [ stroke "white" ]


plainPyramid =
    pyramid [ stroke "grey" ]


pyramid : List (Attribute Msg) -> Size -> Float -> Float -> Svg Msg
pyramid attributes size x y =
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
            ([ d dString
             , strokeWidth "2"
             , fillOpacity "0.4"
             ]
                ++ attributes
            )
            []
        ]
            ++ getDots size x y
            |> g []


nullSvg =
    Svg.polygon [] []
