module View exposing (view)

import Model exposing (Model, Size(..), Stack(..))
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
            [ renderStack (Single Queen) 140 200
            , renderStack (FullTree) 300 200
            , renderStack (PartialTree) 460 200
            , renderStack (DroneTree) 140 400
            , renderStack (NoDroneTree) 300 400
            , renderStack (FullNest) 460 400
            , renderStack (PartialNest) 140 600
            , renderStack (DroneNest) 300 600
            , renderStack (NoDroneNest) 460 600
            ]
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


getDots : Size -> Float -> Float -> List (Svg Msg)
getDots size x y =
    let
        scale =
            getScale size

        pawnScale =
            getScale Pawn
    in
        [ ellipse
            [ (x + (-scale * 5 / 12))
                |> toString
                |> cx
            , (y + -scale)
                |> toString
                |> cy
            , pawnScale
                / 7
                |> toString
                |> rx
            , pawnScale
                / 5
                |> toString
                |> ry
            , stroke "#EEEEEE"
            , fillOpacity "0.0"
            ]
            []
        ]


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
