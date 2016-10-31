module Msg exposing (..)

import Material
import Model exposing (Size, BoardId)


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp
    | Place BoardId
    | PlaceOnEdge BoardId
    | Select (Maybe Size)
