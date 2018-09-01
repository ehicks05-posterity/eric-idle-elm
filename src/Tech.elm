module Tech exposing (Tech, farming, stoneConstruction, techs, wheel, woodConstruction)

import Util exposing (..)


techs =
    [ farming
    , woodConstruction
    , stoneConstruction
    , wheel
    ]


type alias Tech =
    { name : String
    , cost : List ResourceCost
    , image : String
    , prereq : String

    -- mutable
    , discovered : Bool
    , status : DisplayStatus
    }


farming =
    Tech "farming" [ ResourceCost "research" 1 ] "enlightenment.png" "unlockLevelOneTech" False Hidden


woodConstruction =
    Tech "woodConstruction" [ ResourceCost "research" 2 ] "enlightenment.png" "unlockLevelOneTech" False Hidden


stoneConstruction =
    Tech "stoneConstruction" [ ResourceCost "research" 5 ] "enlightenment.png" "unlockLevelOneTech" False Hidden


wheel =
    Tech "wheel" [ ResourceCost "research" 5 ] "enlightenment.png" "unlockLevelOneTech" False Hidden
