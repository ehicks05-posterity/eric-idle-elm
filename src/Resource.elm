module Resource exposing (Resource, defaultResource, food, getByName, leather, lumber, research, resources, stone, villagers)

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

    -- mutable
    , status : DisplayStatus
    , amount : Float
    }


defaultResource =
    Resource "default" 0 "default.png" "noPreReq" Shown 0


food =
    Resource "food" 40 "wheat.png" "noPreReq" Shown 0


villagers =
    Resource "villagers" 0 "backup.png" "unlockVillagers" Hidden 0


lumber =
    Resource "lumber" 24 "wood-pile.png" "unlockWoodConstruction" Hidden 0


research =
    Resource "research" 20 "coma.png" "unlockVillagers" Hidden 0


stone =
    Resource "stone" 10 "stone-pile.png" "unlockStoneConstruction" Hidden 0


leather =
    Resource "leather" 20 "animal-hide.png" "unlockHunting" Hidden 0


getByName : String -> List Resource -> Resource
getByName name list =
    case List.head (List.filter (\r -> r.name == name) list) of
        Just r ->
            r

        Nothing ->
            defaultResource
