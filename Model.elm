module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model }


defaultState =
    { mdl = Material.model }


type Size
    = Queen
    | Drone
    | Pawn


type Stack
    = Single Size
    | FullTree
    | PartialTree
    | DroneTree
    | NoDroneTree
    | FullNest
    | PartialNest
    | DroneNest
    | NoDroneNest
