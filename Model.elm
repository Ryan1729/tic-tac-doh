module Model exposing (..)

import Material


type alias Model =
    { mdl : Material.Model
    , selected : Maybe Size
    , board : Board
    , stash : Stash
    , player : Player
    , outcome : Outcome
    }


defaultState =
    { mdl = Material.model
    , selected = Just Pawn
    , board = EmptyBoard
    , stash = defaultStash
    , player = User
    , outcome = TBD
    }


type Player
    = User
    | CPU


nextPlayer player =
    case player of
        User ->
            CPU

        CPU ->
            User


type Outcome
    = UserWin
    | UserWinByExhaustion
    | CPUWin
    | CPUWinByExhaustion
    | Tie
    | TBD


getOutcome : Model -> Outcome
getOutcome model =
    if noLegalMoves model then
        case model.player of
            User ->
                CPUWinByExhaustion

            CPU ->
                UserWinByExhaustion
    else
        case checkForWinner model of
            TBD ->
                if stashIsEmpty model.stash then
                    Tie
                else
                    TBD

            result ->
                result


checkForWinner : Model -> Outcome
checkForWinner model =
    case model.board of
        ThreeByThree r ->
            checkLine r.zeroZero r.zeroOne r.zeroTwo
                |> andCheckLine r.oneZero r.oneOne r.oneTwo
                |> andCheckLine r.twoZero r.twoOne r.twoTwo
                |> andCheckLine r.zeroZero r.oneZero r.twoZero
                |> andCheckLine r.zeroOne r.oneOne r.twoOne
                |> andCheckLine r.zeroTwo r.oneTwo r.twoTwo
                |> andCheckLine r.zeroZero r.oneOne r.twoTwo
                |> andCheckLine r.zeroTwo r.oneOne r.twoZero
                |> subOutcomeToOutcome model.player

        ThreeByTwo r ->
            checkLine r.zeroZero r.oneZero r.twoZero
                |> andCheckLine r.zeroOne r.oneOne r.twoOne
                |> subOutcomeToOutcome model.player

        TwoByThree r ->
            checkLine r.zeroZero r.zeroOne r.zeroTwo
                |> andCheckLine r.oneZero r.oneOne r.oneTwo
                |> subOutcomeToOutcome model.player

        OneByThree s1 s2 s3 ->
            checkLine s1 s2 s3
                |> subOutcomeToOutcome model.player

        ThreeByOne s1 s2 s3 ->
            checkLine s1 s2 s3
                |> subOutcomeToOutcome model.player

        _ ->
            TBD


noLegalMoves : Model -> Bool
noLegalMoves model =
    let
        hasLegalMoves =
            canPlaceQueen model || canPlaceDrone model || canPlacePawn model
    in
        not hasLegalMoves


canPlaceQueen =
    canPlace Queen


canPlaceDrone =
    canPlace Drone


canPlacePawn =
    canPlace Pawn


canPlace : Size -> Model -> Bool
canPlace size model =
    if stashGet size model.stash <= 0 then
        False
    else
        case model.board of
            ThreeByThree r ->
                threeByThreeAny (sizeFits size) r

            _ ->
                True


threeByThreeAny : (Stack -> Bool) -> ThreeByThreeRecord -> Bool
threeByThreeAny predicate record =
    predicate record.zeroZero
        || predicate record.zeroOne
        || predicate record.zeroTwo
        || predicate record.oneZero
        || predicate record.oneOne
        || predicate record.oneTwo
        || predicate record.twoOne
        || predicate record.twoZero
        || predicate record.twoTwo


type SubOutcome
    = Win
    | Undetermined


subOutcomeToOutcome : Player -> SubOutcome -> Outcome
subOutcomeToOutcome player subOutcome =
    case subOutcome of
        Undetermined ->
            TBD

        Win ->
            case player of
                User ->
                    UserWin

                CPU ->
                    CPUWin


checkLine : Stack -> Stack -> Stack -> SubOutcome
checkLine stack1 stack2 stack3 =
    case ( stack1, stack2, stack3 ) of
        ( EmptyStack, _, _ ) ->
            Undetermined

        ( _, EmptyStack, _ ) ->
            Undetermined

        ( _, _, EmptyStack ) ->
            Undetermined

        ( Single size1, Single size2, Single size3 ) ->
            checkSizes size1 size2 size3

        ( FullTree, FullTree, FullTree ) ->
            Win

        ( FullTree, _, _ ) ->
            checkLine (Single Queen) stack2 stack3
                |> andCheckLine (Single Drone) stack2 stack3
                |> andCheckLine (Single Pawn) stack2 stack3

        ( _, FullTree, _ ) ->
            checkLine stack1 (Single Queen) stack3
                |> andCheckLine stack1 (Single Drone) stack3
                |> andCheckLine stack1 (Single Pawn) stack3

        ( _, _, FullTree ) ->
            checkLine stack1 stack2 (Single Queen)
                |> andCheckLine stack1 stack2 (Single Drone)
                |> andCheckLine stack1 stack2 (Single Pawn)

        ( PartialTree, PartialTree, PartialTree ) ->
            Win

        ( PartialTree, _, _ ) ->
            checkLine (Single Queen) stack2 stack3
                |> andCheckLine (Single Drone) stack2 stack3

        ( _, PartialTree, _ ) ->
            checkLine stack1 (Single Queen) stack3
                |> andCheckLine stack1 (Single Drone) stack3

        ( _, _, PartialTree ) ->
            checkLine stack1 stack2 (Single Queen)
                |> andCheckLine stack1 stack2 (Single Drone)

        ( DroneTree, DroneTree, DroneTree ) ->
            Win

        ( DroneTree, _, _ ) ->
            checkLine (Single Drone) stack2 stack3
                |> andCheckLine (Single Pawn) stack2 stack3

        ( _, DroneTree, _ ) ->
            checkLine stack1 (Single Drone) stack3
                |> andCheckLine stack1 (Single Pawn) stack3

        ( _, _, DroneTree ) ->
            checkLine stack1 stack2 (Single Drone)
                |> andCheckLine stack1 stack2 (Single Pawn)

        ( NoDroneTree, NoDroneTree, NoDroneTree ) ->
            Win

        ( NoDroneTree, _, _ ) ->
            checkLine (Single Queen) stack2 stack3
                |> andCheckLine (Single Pawn) stack2 stack3

        ( _, NoDroneTree, _ ) ->
            checkLine stack1 (Single Queen) stack3
                |> andCheckLine stack1 (Single Pawn) stack3

        ( _, _, NoDroneTree ) ->
            checkLine stack1 stack2 (Single Queen)
                |> andCheckLine stack1 stack2 (Single Pawn)

        ( FullNest, FullNest, FullNest ) ->
            Win

        ( FullNest, _, _ ) ->
            checkLine (Single Queen) stack2 stack3

        ( _, FullNest, _ ) ->
            checkLine stack1 (Single Queen) stack3

        ( _, _, FullNest ) ->
            checkLine stack1 stack2 (Single Queen)

        ( PartialNest, PartialNest, PartialNest ) ->
            Win

        ( PartialNest, _, _ ) ->
            checkLine (Single Drone) stack2 stack3

        ( _, PartialNest, _ ) ->
            checkLine stack1 (Single Drone) stack3

        ( _, _, PartialNest ) ->
            checkLine stack1 stack2 (Single Drone)

        ( DroneNest, DroneNest, DroneNest ) ->
            Win

        ( DroneNest, _, _ ) ->
            checkLine (Single Queen) stack2 stack3

        ( _, DroneNest, _ ) ->
            checkLine stack1 (Single Queen) stack3

        ( _, _, DroneNest ) ->
            checkLine stack1 stack2 (Single Queen)

        ( NoDroneNest, NoDroneNest, NoDroneNest ) ->
            Win

        ( NoDroneNest, _, _ ) ->
            checkLine (Single Queen) stack2 stack3

        ( _, NoDroneNest, _ ) ->
            checkLine stack1 (Single Queen) stack3

        ( _, _, NoDroneNest ) ->
            checkLine stack1 stack2 (Single Queen)


andCheckLine : Stack -> Stack -> Stack -> SubOutcome -> SubOutcome
andCheckLine stack1 stack2 stack3 outcome =
    case outcome of
        Undetermined ->
            checkLine stack1 stack2 stack3

        predetermined ->
            predetermined


type Size
    = Queen
    | Drone
    | Pawn


checkSizes : Size -> Size -> Size -> SubOutcome
checkSizes size1 size2 size3 =
    if size1 == size2 && size2 == size3 then
        Win
    else
        Undetermined


andCheckSize : Size -> Size -> Size -> SubOutcome -> SubOutcome
andCheckSize size1 size2 size3 subOutcome =
    case subOutcome of
        Undetermined ->
            checkSizes size1 size2 size3

        predetermined ->
            predetermined


type alias Stash =
    { queen : Int
    , drone : Int
    , pawn : Int
    }


maxStashAmount =
    5


defaultStash =
    Stash maxStashAmount maxStashAmount maxStashAmount


getAvailableSizes : Stash -> List Size
getAvailableSizes stash =
    [ Queen, Drone, Pawn ]
        |> List.filter (\size -> stashGet size stash > 0)


stashIsEmpty : Stash -> Bool
stashIsEmpty { queen, drone, pawn } =
    queen <= 0 && drone <= 0 && pawn <= 0


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
        , oneZero : Stack
        , zeroOne : Stack
        , oneOne : Stack
        , zeroTwo : Stack
        , oneTwo : Stack
        }
    | ThreeByTwo
        { zeroZero : Stack
        , oneZero : Stack
        , twoZero : Stack
        , zeroOne : Stack
        , oneOne : Stack
        , twoOne : Stack
        }
    | ThreeByThree ThreeByThreeRecord


type alias ThreeByThreeRecord =
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


twoByTwo zeroZero oneZero zeroOne oneOne =
    TwoByTwo
        { zeroZero = zeroZero
        , oneZero = oneZero
        , zeroOne = zeroOne
        , oneOne = oneOne
        }


twoByThree zeroZero oneZero zeroOne oneOne zeroTwo oneTwo =
    TwoByThree
        { zeroZero = zeroZero
        , oneZero = oneZero
        , zeroOne = zeroOne
        , oneOne = oneOne
        , zeroTwo = zeroTwo
        , oneTwo = oneTwo
        }


threeByTwo zeroZero oneZero twoZero zeroOne oneOne twoOne =
    ThreeByTwo
        { zeroZero = zeroZero
        , oneZero = oneZero
        , twoZero = twoZero
        , zeroOne = zeroOne
        , oneOne = oneOne
        , twoOne = twoOne
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


getAvailableBoardIdSizePairs : Board -> Stash -> List ( BoardId, Size )
getAvailableBoardIdSizePairs board stash =
    let
        availableSizes =
            getAvailableSizes stash

        potentialBoardIds =
            getPotentialBoardIds board
    in
        List.concatMap
            (\boardId ->
                case get boardId board of
                    Just currentStack ->
                        List.filterMap
                            (\size ->
                                if sizeFits size currentStack then
                                    Just ( boardId, size )
                                else
                                    Nothing
                            )
                            availableSizes

                    Nothing ->
                        []
            )
            potentialBoardIds


getPotentialBoardIds : Board -> List BoardId
getPotentialBoardIds board =
    case board of
        EmptyBoard ->
            [ ZeroZero ]

        OneByOne _ ->
            [ ZeroZero ]

        OneByTwo _ _ ->
            [ ZeroZero, ZeroOne ]

        TwoByOne _ _ ->
            [ ZeroZero, OneZero ]

        OneByThree _ _ _ ->
            [ ZeroZero, ZeroOne, ZeroTwo ]

        ThreeByOne _ _ _ ->
            [ ZeroZero, OneZero, TwoZero ]

        TwoByTwo _ ->
            [ ZeroZero
            , ZeroOne
            , OneZero
            , OneOne
            ]

        TwoByThree _ ->
            [ ZeroZero
            , OneZero
            , ZeroOne
            , OneOne
            , ZeroTwo
            , OneTwo
            ]

        ThreeByTwo _ ->
            [ ZeroZero
            , OneZero
            , TwoZero
            , ZeroOne
            , OneOne
            , TwoOne
            ]

        ThreeByThree _ ->
            [ ZeroZero
            , ZeroOne
            , ZeroTwo
            , OneZero
            , OneOne
            , OneTwo
            , TwoOne
            , TwoZero
            , TwoTwo
            ]


place : BoardId -> Size -> Board -> Board
place boardId size board =
    case get (Debug.log "placeboardId" boardId) board of
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


getAvailableEdgeIdSizePairs : Board -> Stash -> List ( EdgeId, Size )
getAvailableEdgeIdSizePairs board stash =
    let
        availableSizes =
            getAvailableSizes stash

        availableEdgeIds =
            getAvailableEdgeIds board
    in
        List.concatMap
            (\edgeId ->
                List.map ((,) edgeId) availableSizes
            )
            availableEdgeIds


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


getAvailableEdgeIds : Board -> List EdgeId
getAvailableEdgeIds board =
    case board of
        EmptyBoard ->
            [ EdgeZeroZero ]

        OneByOne _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeZeroOne
            , EdgeTwoOne
            , EdgeZeroTwo
            , EdgeOneTwo
            , EdgeTwoTwo
            ]

        OneByTwo _ _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeZeroOne
            , EdgeTwoOne
            , EdgeZeroTwo
            , EdgeTwoTwo
            , EdgeZeroThree
            , EdgeOneThree
            , EdgeTwoThree
            ]

        TwoByOne _ _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeThreeZero
            , EdgeZeroOne
            , EdgeThreeOne
            , EdgeZeroTwo
            , EdgeOneTwo
            , EdgeTwoTwo
            , EdgeThreeTwo
            ]

        OneByThree _ _ _ ->
            [ EdgeZeroZero
            , EdgeTwoZero
            , EdgeZeroOne
            , EdgeTwoOne
            , EdgeZeroTwo
            , EdgeTwoTwo
            ]

        ThreeByOne _ _ _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeZeroTwo
            , EdgeOneTwo
            , EdgeTwoTwo
            ]

        TwoByTwo _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeThreeZero
            , EdgeZeroOne
            , EdgeThreeOne
            , EdgeZeroTwo
            , EdgeThreeTwo
            , EdgeZeroThree
            , EdgeOneThree
            , EdgeTwoThree
            , EdgeThreeThree
            ]

        TwoByThree _ ->
            [ EdgeZeroZero
            , EdgeThreeZero
            , EdgeZeroOne
            , EdgeThreeOne
            , EdgeZeroTwo
            , EdgeThreeTwo
            ]

        ThreeByTwo _ ->
            [ EdgeZeroZero
            , EdgeOneZero
            , EdgeTwoZero
            , EdgeZeroThree
            , EdgeOneThree
            , EdgeTwoThree
            ]

        ThreeByThree _ ->
            []


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

                EdgeZeroTwo ->
                    twoByTwo EmptyStack stack (Single size) EmptyStack

                EdgeOneTwo ->
                    OneByTwo stack (Single size)

                EdgeTwoTwo ->
                    twoByTwo stack EmptyStack EmptyStack (Single size)

                _ ->
                    board

        OneByTwo s1 s2 ->
            case boardId of
                EdgeZeroZero ->
                    twoByThree (Single size) EmptyStack EmptyStack s1 EmptyStack s2

                EdgeOneZero ->
                    OneByThree (Single size) s1 s2

                EdgeTwoZero ->
                    twoByThree EmptyStack (Single size) s1 EmptyStack s2 EmptyStack

                EdgeZeroOne ->
                    twoByTwo (Single size) s1 EmptyStack s2

                EdgeTwoOne ->
                    twoByTwo s1 (Single size) s2 EmptyStack

                EdgeZeroTwo ->
                    twoByTwo EmptyStack s1 (Single size) s2

                EdgeTwoTwo ->
                    twoByTwo s1 EmptyStack s2 (Single size)

                EdgeZeroThree ->
                    twoByThree EmptyStack s1 EmptyStack s2 (Single size) EmptyStack

                EdgeOneThree ->
                    OneByThree s1 s2 (Single size)

                EdgeTwoThree ->
                    twoByThree s1 EmptyStack s2 EmptyStack EmptyStack (Single size)

                _ ->
                    board

        TwoByOne s1 s2 ->
            case boardId of
                EdgeZeroZero ->
                    threeByTwo (Single size) EmptyStack EmptyStack EmptyStack s1 s2

                EdgeOneZero ->
                    twoByTwo (Single size) EmptyStack s1 s2

                EdgeTwoZero ->
                    twoByTwo EmptyStack (Single size) s1 s2

                EdgeThreeZero ->
                    threeByTwo EmptyStack EmptyStack (Single size) s1 s2 EmptyStack

                EdgeZeroOne ->
                    ThreeByOne (Single size) s1 s2

                EdgeThreeOne ->
                    ThreeByOne s1 s2 (Single size)

                EdgeZeroTwo ->
                    threeByTwo EmptyStack s1 s2 (Single size) EmptyStack EmptyStack

                EdgeOneTwo ->
                    twoByTwo s1 s2 (Single size) EmptyStack

                EdgeTwoTwo ->
                    twoByTwo s1 s2 EmptyStack (Single size)

                EdgeThreeTwo ->
                    threeByTwo s1 s2 EmptyStack EmptyStack EmptyStack (Single size)

                _ ->
                    board

        OneByThree s1 s2 s3 ->
            case boardId of
                EdgeZeroZero ->
                    twoByThree (Single size) s1 EmptyStack s2 EmptyStack s3

                EdgeTwoZero ->
                    twoByThree s1 (Single size) s2 EmptyStack s3 EmptyStack

                EdgeZeroOne ->
                    twoByThree EmptyStack s1 (Single size) s2 EmptyStack s3

                EdgeTwoOne ->
                    twoByThree s1 EmptyStack s2 (Single size) s3 EmptyStack

                EdgeZeroTwo ->
                    twoByThree EmptyStack s1 EmptyStack s2 (Single size) s3

                EdgeTwoTwo ->
                    twoByThree s1 EmptyStack s2 EmptyStack s3 (Single size)

                _ ->
                    board

        ThreeByOne s1 s2 s3 ->
            case boardId of
                EdgeZeroZero ->
                    threeByTwo (Single size) EmptyStack EmptyStack s1 s2 s3

                EdgeOneZero ->
                    threeByTwo EmptyStack (Single size) EmptyStack s1 s2 s3

                EdgeTwoZero ->
                    threeByTwo EmptyStack EmptyStack (Single size) s1 s2 s3

                EdgeZeroTwo ->
                    threeByTwo s1 s2 s3 (Single size) EmptyStack EmptyStack

                EdgeOneTwo ->
                    threeByTwo s1 s2 s3 EmptyStack (Single size) EmptyStack

                EdgeTwoTwo ->
                    threeByTwo s1 s2 s3 EmptyStack EmptyStack (Single size)

                _ ->
                    board

        TwoByTwo r ->
            let
                s1 =
                    r.zeroZero

                s2 =
                    r.oneZero

                s3 =
                    r.zeroOne

                s4 =
                    r.oneOne
            in
                case boardId of
                    EdgeZeroZero ->
                        threeByThree (Single size) EmptyStack EmptyStack EmptyStack s1 s2 EmptyStack s3 s4

                    EdgeOneZero ->
                        twoByThree (Single size) EmptyStack s1 s2 s3 s4

                    EdgeTwoZero ->
                        twoByThree EmptyStack (Single size) s1 s2 s3 s4

                    EdgeThreeZero ->
                        threeByThree EmptyStack EmptyStack (Single size) s1 s2 EmptyStack s3 s4 EmptyStack

                    EdgeZeroOne ->
                        threeByTwo (Single size) s1 s2 EmptyStack s3 s4

                    EdgeThreeOne ->
                        threeByTwo s1 s2 (Single size) s3 s4 EmptyStack

                    EdgeZeroTwo ->
                        threeByTwo EmptyStack s1 s2 (Single size) s3 s4

                    EdgeThreeTwo ->
                        threeByTwo s1 s2 EmptyStack s3 s4 (Single size)

                    EdgeZeroThree ->
                        threeByThree EmptyStack s1 s2 EmptyStack s3 s4 (Single size) EmptyStack EmptyStack

                    EdgeOneThree ->
                        twoByThree s1 s2 s3 s4 (Single size) EmptyStack

                    EdgeTwoThree ->
                        twoByThree s1 s2 s3 s4 EmptyStack (Single size)

                    EdgeThreeThree ->
                        threeByThree s1 s2 EmptyStack s3 s4 EmptyStack EmptyStack EmptyStack (Single size)

                    _ ->
                        board

        TwoByThree r ->
            let
                s1 =
                    r.zeroZero

                s2 =
                    r.oneZero

                s3 =
                    r.zeroOne

                s4 =
                    r.oneOne

                s5 =
                    r.zeroTwo

                s6 =
                    r.oneTwo
            in
                case boardId of
                    EdgeZeroZero ->
                        threeByThree (Single size) s1 s2 EmptyStack s3 s4 EmptyStack s5 s6

                    EdgeThreeZero ->
                        threeByThree s1 s2 (Single size) s3 s4 EmptyStack s5 s6 EmptyStack

                    EdgeZeroOne ->
                        threeByThree EmptyStack s1 s2 (Single size) s3 s4 EmptyStack s5 s6

                    EdgeThreeOne ->
                        threeByThree s1 s2 EmptyStack s3 s4 (Single size) s5 s6 EmptyStack

                    EdgeZeroTwo ->
                        threeByThree EmptyStack s1 s2 EmptyStack s3 s4 (Single size) s5 s6

                    EdgeThreeTwo ->
                        threeByThree s1 s2 EmptyStack s3 s4 EmptyStack s5 s6 (Single size)

                    _ ->
                        board

        ThreeByTwo r ->
            let
                s1 =
                    r.zeroZero

                s2 =
                    r.oneZero

                s3 =
                    r.twoZero

                s4 =
                    r.zeroOne

                s5 =
                    r.oneOne

                s6 =
                    r.twoOne
            in
                case boardId of
                    EdgeZeroZero ->
                        threeByThree (Single size) EmptyStack EmptyStack s1 s2 s3 s4 s5 s6

                    EdgeOneZero ->
                        threeByThree EmptyStack (Single size) EmptyStack s1 s2 s3 s4 s5 s6

                    EdgeTwoZero ->
                        threeByThree EmptyStack EmptyStack (Single size) s1 s2 s3 s4 s5 s6

                    EdgeZeroThree ->
                        threeByThree s1 s2 s3 s4 s5 s6 (Single size) EmptyStack EmptyStack

                    EdgeOneThree ->
                        threeByThree s1 s2 s3 s4 s5 s6 EmptyStack (Single size) EmptyStack

                    EdgeTwoThree ->
                        threeByThree s1 s2 s3 s4 s5 s6 EmptyStack EmptyStack (Single size)

                    _ ->
                        board

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

                OneZero ->
                    Just r.oneZero

                OneOne ->
                    Just r.oneOne

                ZeroTwo ->
                    Just r.zeroTwo

                OneTwo ->
                    Just r.oneTwo

                _ ->
                    Nothing

        ThreeByTwo r ->
            case boardId of
                ZeroZero ->
                    Just r.zeroZero

                ZeroOne ->
                    Just r.oneZero

                ZeroTwo ->
                    Just r.twoZero

                OneZero ->
                    Just r.zeroOne

                OneOne ->
                    Just r.oneOne

                OneTwo ->
                    Just r.twoOne

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
    let
        _ =
            Debug.log "boardId" boardId
    in
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

                    OneZero ->
                        TwoByThree { r | oneZero = stack }

                    OneOne ->
                        TwoByThree { r | oneOne = stack }

                    ZeroTwo ->
                        TwoByThree { r | zeroTwo = stack }

                    OneTwo ->
                        TwoByThree { r | oneTwo = stack }

                    _ ->
                        board

            ThreeByTwo r ->
                case boardId of
                    ZeroZero ->
                        ThreeByTwo { r | zeroZero = stack }

                    ZeroOne ->
                        ThreeByTwo { r | oneZero = stack }

                    ZeroTwo ->
                        ThreeByTwo { r | twoZero = stack }

                    OneZero ->
                        ThreeByTwo { r | zeroOne = stack }

                    OneOne ->
                        ThreeByTwo { r | oneOne = stack }

                    OneTwo ->
                        ThreeByTwo { r | twoOne = stack }

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
