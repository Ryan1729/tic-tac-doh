module Msg exposing (..)

import Material
import Model exposing (Size, BoardId, EdgeId)


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp
    | Place BoardId
    | PlaceOnEdge EdgeId
    | Select (Maybe Size)
