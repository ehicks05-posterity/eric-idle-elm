module Util exposing (DisplayStatus(..), LockStatus(..), ResourceCost, ResourceEffect(..), ResourceModifierType(..), getResourceCostByResource)


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


getResourceCostByResource : List ResourceCost -> String -> ResourceCost
getResourceCostByResource list resourceName =
    case List.head (List.filter (\rc -> rc.resource == resourceName) list) of
        Just rc ->
            rc

        Nothing ->
            ResourceCost "default" 0


type ResourceModifierType
    = Additive
    | Multiplicative


type ResourceEffect
    = ResourceLimitMod String ResourceModifierType Float
    | ResourceProductionMod String ResourceModifierType Float
