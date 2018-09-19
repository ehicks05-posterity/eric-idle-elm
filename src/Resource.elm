module Resource exposing (Resource, defaultResource, food, leather, lumber, research, resources, stone, villagers)

import Util exposing (..)


resources =
    [ food
    , villagers
    , lumber
    , research
    , stone
    , leather
    ]


type alias Resource =
    { name : String
    , baseLimit : Float
    , image : String
    , prereq : String
    , index : Int

    -- mutable
    , status : DisplayStatus
    , amount : Float
    }


defaultResource =
    Resource "default" 0 "default" "noPreReq" 0 Shown 0


food =
    Resource "food" 40 "wheat" "noPreReq" 1 Shown 0


villagers =
    Resource "villagers" 0 "person" "unlockVillagers" 2 Hidden 0


lumber =
    Resource "lumber" 24 "log" "unlockWoodConstruction" 3 Hidden 0


research =
    Resource "research" 20 "vial" "unlockVillagers" 4 Hidden 0


stone =
    Resource "stone" 10 "stone-block" "unlockStoneConstruction" 5 Hidden 0


leather =
    Resource "leather" 20 "animal-hide" "unlockHunting" 6 Hidden 0
