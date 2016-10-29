module Msg exposing (..)

import Material
import Model exposing (Size)


type Msg
    = Mdl (Material.Msg Msg)
    | NoOp
    | Place
    | Select (Maybe Size)
