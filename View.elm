module View exposing (view)

import Model exposing (Model, Size(..))
import Html exposing (Html, text)
import Msg exposing (Msg(..))
import Material.Button as Button
import Svg exposing (Svg, svg, circle, polygon, Attribute)
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
            [ pyramid Pawn 140 100
            , pyramid Drone 300 100
            , pyramid Queen 460 100
            ]
        ]


pawnPyramidPathSuffix =
    pyramidPathSuffix 30


dronePyramidPathSuffix =
    pyramidPathSuffix 40


queenPyramidPathSuffix =
    pyramidPathSuffix (160 / 3)


pyramidPathSuffix : Float -> String
pyramidPathSuffix scale =
    let
        sclaeString =
            toString scale

        doubleSclaeString =
            toString (2 * scale)
    in
        (" l -" ++ sclaeString ++ " " ++ doubleSclaeString)
            ++ (" l " ++ sclaeString ++ " " ++ sclaeString)
            ++ (" l " ++ sclaeString ++ " -" ++ sclaeString)
            ++ ("l -" ++ sclaeString ++ " -" ++ doubleSclaeString)
            ++ (" l 0 " ++ toString (3 * scale))


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
        Svg.path
            [ d dString
            , stroke "grey"
            , strokeWidth "2"
            ]
            []
