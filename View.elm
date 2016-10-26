module View exposing (view)

import Model exposing (Model, Size(..))
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
            [ pyramid Pawn 140 400
            , pyramid Drone 300 400
            , pyramid Pawn 300 (below Drone 400)
              --nest
            , pyramid Queen 460 400
            , pyramid Drone 460 (below Queen 400)
            , pyramid Pawn 460 (below Drone (below Queen 400))
              --tree
            , pyramid Queen 140 200
            , pyramid Drone 140 (above Drone 200)
            , pyramid Pawn 140 (above Pawn (above Drone 200))
            ]
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
            , stroke "#888888"
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
            , fillOpacity "0.0"
            ]
            []
        ]
            ++ getDots size x y
            |> g []
