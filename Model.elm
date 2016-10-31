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
    , board =
        OneByTwo FullTree EmptyStack
        -- FullNest
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


maxStashAmount =
    5


defaultStash =
    Stash maxStashAmount maxStashAmount maxStashAmount


stashGet : Size -> Stash -> Int
stashGet size stash =
    case size of
        Queen ->
            stash.queen

        Drone ->
            stash.drone

        Pawn ->
            stash.pawn


stashSet : Size -> Int -> Stash -> Stash
stashSet size amount stash =
    case size of
        Queen ->
            { stash | queen = amount }

        Drone ->
            { stash | drone = amount }

        Pawn ->
            { stash | pawn = amount }


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
    | TwoByOne Stack Stack
    | OneByThree Stack Stack Stack
    | ThreeByOne Stack Stack Stack
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


threeByThree zeroZero zeroOne zeroTwo oneZero oneOne oneTwo twoZero twoOne twoTwo =
    ThreeByThree
        { zeroZero = zeroZero
        , zeroOne = zeroOne
        , zeroTwo = zeroTwo
        , oneZero = oneZero
        , oneOne = oneOne
        , oneTwo = oneTwo
        , twoZero = twoZero
        , twoOne = twoOne
        , twoTwo = twoTwo
        }


type BoardId
    = ZeroZero
    | ZeroOne
    | ZeroTwo
    | OneZero
    | OneOne
    | OneTwo
    | TwoZero
    | TwoOne
    | TwoTwo


place : BoardId -> Size -> Board -> Board
place boardId size board =
    case get boardId board of
        Just stack ->
            set boardId (placeOnStack size stack) board

        Nothing ->
            board


placeOnStack : Size -> Stack -> Stack
placeOnStack size stack =
    if sizeFits size stack then
        case stack of
            EmptyStack ->
                Single size

            Single currentSize ->
                case ( size, currentSize ) of
                    ( Pawn, Drone ) ->
                        DroneTree

                    ( Pawn, Queen ) ->
                        NoDroneTree

                    ( Drone, Pawn ) ->
                        PartialNest

                    ( Drone, Queen ) ->
                        PartialTree

                    ( Queen, Pawn ) ->
                        NoDroneNest

                    ( Queen, Drone ) ->
                        DroneNest

                    _ ->
                        stack

            PartialTree ->
                FullTree

            PartialNest ->
                FullNest

            _ ->
                stack
    else
        stack


type EdgeId
    = EdgeZeroZero
    | EdgeZeroOne
    | EdgeZeroTwo
    | EdgeZeroThree
    | EdgeOneZero
    | EdgeOneOne
    | EdgeOneTwo
    | EdgeOneThree
    | EdgeTwoZero
    | EdgeTwoOne
    | EdgeTwoTwo
    | EdgeTwoThree
    | EdgeThreeZero
    | EdgeThreeOne
    | EdgeThreeTwo
    | EdgeThreeThree


placeOnEdge : EdgeId -> Size -> Board -> Board
placeOnEdge boardId size board =
    case board of
        EmptyBoard ->
            board

        OneByOne stack ->
            case boardId of
                EdgeZeroZero ->
                    twoByTwo (Single size) EmptyStack EmptyStack stack

                EdgeOneZero ->
                    OneByTwo (Single size) stack

                EdgeTwoZero ->
                    twoByTwo EmptyStack (Single size) stack EmptyStack

                EdgeZeroOne ->
                    TwoByOne (Single size) stack

                EdgeTwoOne ->
                    TwoByOne stack (Single size)

                EdgeOneTwo ->
                    OneByTwo stack (Single size)

                EdgeZeroTwo ->
                    twoByTwo EmptyStack stack (Single size) EmptyStack

                EdgeTwoTwo ->
                    twoByTwo stack EmptyStack EmptyStack (Single size)

                _ ->
                    board

        -- OneByTwo s1 s2 ->
        -- TwoByOne Stack Stack
        -- OneByThree Stack Stack Stack
        -- ThreeByOne Stack Stack Stack
        -- TwoByTwo
        --     { zeroZero : Stack
        --     , zeroOne : Stack
        --     , oneZero : Stack
        --     , oneOne : Stack
        --     }
        -- TwoByThree
        --     { zeroZero : Stack
        --     , zeroOne : Stack
        --     , zeroTwo : Stack
        --     , oneZero : Stack
        --     , oneOne : Stack
        --     , oneTwo : Stack
        --     }
        -- ThreeByThree
        --     { zeroZero : Stack
        --     , zeroOne : Stack
        --     , zeroTwo : Stack
        --     , oneZero : Stack
        --     , oneOne : Stack
        --     , oneTwo : Stack
        --     , twoOne : Stack
        --     , twoZero : Stack
        --     , twoTwo : Stack
        --     }
        _ ->
            board


get : BoardId -> Board -> Maybe Stack
get boardId board =
    case board of
        EmptyBoard ->
            case boardId of
                ZeroZero ->
                    Just EmptyStack

                _ ->
                    Nothing

        OneByOne stack ->
            case boardId of
                ZeroZero ->
                    Just stack

                _ ->
                    Nothing

        OneByTwo s0 s1 ->
            case boardId of
                ZeroZero ->
                    Just s0

                ZeroOne ->
                    Just s1

                _ ->
                    Nothing

        OneByThree s0 s1 s2 ->
            case boardId of
                ZeroZero ->
                    Just s0

                ZeroOne ->
                    Just s1

                ZeroTwo ->
                    Just s2

                _ ->
                    Nothing

        TwoByOne s0 s1 ->
            case boardId of
                ZeroZero ->
                    Just s0

                OneZero ->
                    Just s1

                _ ->
                    Nothing

        ThreeByOne s0 s1 s2 ->
            case boardId of
                ZeroZero ->
                    Just s0

                OneZero ->
                    Just s1

                TwoZero ->
                    Just s2

                _ ->
                    Nothing

        TwoByTwo r ->
            case boardId of
                ZeroZero ->
                    Just r.zeroZero

                ZeroOne ->
                    Just r.zeroOne

                OneZero ->
                    Just r.oneZero

                OneOne ->
                    Just r.oneOne

                _ ->
                    Nothing

        TwoByThree r ->
            case boardId of
                ZeroZero ->
                    Just r.zeroZero

                ZeroOne ->
                    Just r.zeroOne

                ZeroTwo ->
                    Just r.zeroTwo

                OneZero ->
                    Just r.oneZero

                OneOne ->
                    Just r.oneOne

                OneTwo ->
                    Just r.oneTwo

                _ ->
                    Nothing

        ThreeByThree r ->
            case boardId of
                ZeroZero ->
                    Just r.zeroZero

                ZeroOne ->
                    Just r.zeroOne

                ZeroTwo ->
                    Just r.zeroTwo

                OneZero ->
                    Just r.oneZero

                OneOne ->
                    Just r.oneOne

                OneTwo ->
                    Just r.oneTwo

                TwoZero ->
                    Just r.twoZero

                TwoOne ->
                    Just r.twoOne

                TwoTwo ->
                    Just r.twoTwo


set : BoardId -> Stack -> Board -> Board
set boardId stack board =
    case board of
        EmptyBoard ->
            case boardId of
                ZeroZero ->
                    OneByOne stack

                _ ->
                    board

        OneByOne _ ->
            case boardId of
                ZeroZero ->
                    OneByOne stack

                _ ->
                    board

        OneByTwo s0 s1 ->
            case boardId of
                ZeroZero ->
                    OneByTwo stack s1

                ZeroOne ->
                    OneByTwo s0 stack

                _ ->
                    board

        TwoByOne s0 s1 ->
            case boardId of
                ZeroZero ->
                    TwoByOne stack s1

                OneZero ->
                    TwoByOne s0 stack

                _ ->
                    board

        OneByThree s0 s1 s2 ->
            case boardId of
                ZeroZero ->
                    OneByThree stack s1 s2

                ZeroOne ->
                    OneByThree s0 stack s2

                ZeroTwo ->
                    OneByThree s0 s1 stack

                _ ->
                    board

        ThreeByOne s0 s1 s2 ->
            case boardId of
                ZeroZero ->
                    ThreeByOne stack s1 s2

                OneZero ->
                    ThreeByOne s0 stack s2

                TwoZero ->
                    ThreeByOne s0 s1 stack

                _ ->
                    board

        TwoByTwo r ->
            case boardId of
                ZeroZero ->
                    TwoByTwo { r | zeroZero = stack }

                ZeroOne ->
                    TwoByTwo { r | zeroOne = stack }

                OneZero ->
                    TwoByTwo { r | oneZero = stack }

                OneOne ->
                    TwoByTwo { r | oneOne = stack }

                _ ->
                    board

        TwoByThree r ->
            case boardId of
                ZeroZero ->
                    TwoByThree { r | zeroZero = stack }

                ZeroOne ->
                    TwoByThree { r | zeroOne = stack }

                ZeroTwo ->
                    TwoByThree { r | zeroTwo = stack }

                OneZero ->
                    TwoByThree { r | oneZero = stack }

                OneOne ->
                    TwoByThree { r | oneOne = stack }

                OneTwo ->
                    TwoByThree { r | oneTwo = stack }

                _ ->
                    board

        ThreeByThree r ->
            case boardId of
                ZeroZero ->
                    ThreeByThree { r | zeroZero = stack }

                ZeroOne ->
                    ThreeByThree { r | zeroOne = stack }

                ZeroTwo ->
                    ThreeByThree { r | zeroTwo = stack }

                OneZero ->
                    ThreeByThree { r | oneZero = stack }

                OneOne ->
                    ThreeByThree { r | oneOne = stack }

                OneTwo ->
                    ThreeByThree { r | oneTwo = stack }

                TwoZero ->
                    ThreeByThree { r | twoZero = stack }

                TwoOne ->
                    ThreeByThree { r | twoOne = stack }

                TwoTwo ->
                    ThreeByThree { r | twoTwo = stack }


sizeFits : Size -> Stack -> Bool
sizeFits size stack =
    case stack of
        EmptyStack ->
            True

        Single stackSize ->
            size /= stackSize

        PartialTree ->
            size == Pawn

        PartialNest ->
            size == Queen

        FullTree ->
            False

        DroneTree ->
            False

        NoDroneTree ->
            False

        DroneNest ->
            False

        FullNest ->
            False

        NoDroneNest ->
            False
