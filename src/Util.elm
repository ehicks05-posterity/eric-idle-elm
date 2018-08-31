module Util exposing (..)


type DisplayStatus
    = Hidden
    | Shown


type LockStatus
    = Locked
    | Unlocked


type alias ResourceCost =
    { resource : String
    , amount : Float
    }


type ResourceModifierType
    = Additive
    | Multiplicative


type ResourceEffect
    = ResourceLimitMod String ResourceModifierType Float
    | ResourceProductionMod String ResourceModifierType Float