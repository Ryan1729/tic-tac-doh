module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model
    , selected : Maybe Size
    , board : Board
    , stash : Stash
    }


defaultState =
    { mdl = Material.model
    , selected = Just Pawn
    , board = EmptyBoard
    , stash = defaultStash
    }


type Size
    = Queen
    | Drone
    | Pawn


type alias Stash =
    { queen : Int
    , drone : Int
    , pawn : Int
    }


defaultStash =
    Stash 5 5 5


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
    | OneByTwo Stack Stack
    | OneByThree Stack Stack Stack
    | TwoByTwo
        { zeroZero : Stack
        , zeroOne : Stack
        , oneZero : Stack
        , oneOne : Stack
        }
    | TwoByThree
        { zeroZero : Stack
        , zeroOne : Stack
        , zeroTwo : Stack
        , oneZero : Stack
        , oneOne : Stack
        , oneTwo : Stack
        }
    | ThreeByThree
        { zeroZero : Stack
        , zeroOne : Stack
        , zeroTwo : Stack
        , oneZero : Stack
        , oneOne : Stack
        , oneTwo : Stack
        , twoOne : Stack
        , twoZero : Stack
        , twoTwo : Stack
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
