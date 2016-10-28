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
    = EmptyStack
    | Single Size
    | FullTree
    | PartialTree
    | DroneTree
    | NoDroneTree
    | FullNest
    | PartialNest
    | DroneNest
    | NoDroneNest


type Board
    = EmptyBoard
    | OneByOne Stack
    | OneByTwo (Maybe Stack) (Maybe Stack)
    | OneByThree (Maybe Stack) (Maybe Stack) (Maybe Stack)
    | TwoByTwo
        { zeroZero : Maybe Stack
        , zeroOne : Maybe Stack
        , oneZero : Maybe Stack
        , oneOne : Maybe Stack
        }
    | TwoByThree
        { zeroZero : Maybe Stack
        , zeroOne : Maybe Stack
        , zeroTwo : Maybe Stack
        , oneZero : Maybe Stack
        , oneOne : Maybe Stack
        , oneTwo : Maybe Stack
        }
    | ThreeByThree
        { zeroZero : Maybe Stack
        , zeroOne : Maybe Stack
        , zeroTwo : Maybe Stack
        , oneZero : Maybe Stack
        , oneOne : Maybe Stack
        , oneTwo : Maybe Stack
        , twoOne : Maybe Stack
        , twoZero : Maybe Stack
        , twoTwo : Maybe Stack
        }


twoByTwo zeroZero zeroOne oneZero oneOne =
    TwoByTwo
        { zeroZero = zeroZero
        , zeroOne = zeroOne
        , oneZero = oneZero
        , oneOne = oneOne
        }


twoByThree zeroZero zeroOne zeroTwo oneZero oneOne oneTwo =
    TwoByThree
        { zeroZero = zeroZero
        , zeroOne = zeroOne
        , zeroTwo = zeroTwo
        , oneZero = oneZero
        , oneOne = oneOne
        , oneTwo = oneTwo
        }


threeByThree zeroZero zeroOne zeroTwo oneZero oneOne oneTwo twoOne twoZero twoTwo =
    ThreeByThree
        { zeroZero = zeroZero
        , zeroOne = zeroOne
        , zeroTwo = zeroTwo
        , oneZero = oneZero
        , oneOne = oneOne
        , oneTwo = oneTwo
        , twoOne = twoOne
        , twoZero = twoZero
        , twoTwo = twoTwo
        }
