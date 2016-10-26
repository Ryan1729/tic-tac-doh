module View exposing (view)

import Model exposing (Model)
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
            [ pyramid 300 100
            , pyramid 460 100
            ]
        ]


largePyramidPathSuffix =
    pyramidPathSuffix 70


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


pyramid : Float -> Float -> Svg Msg
pyramid x y =
    let
        dString =
            "M "
                ++ toString x
                ++ " "
                ++ toString y
                ++ largePyramidPathSuffix
    in
        Svg.path
            [ d dString
            , stroke "grey"
            , strokeWidth "2"
            ]
            []
