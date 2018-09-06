module Building exposing (..)

import Util exposing (..)


initialBuildings =
    [ huts
    , farms
    , lumberMills
    , storerooms
    , quarries
    , schools
    , libraries
    ]


type alias Building =
    { name : String
    , image : String
    , prereq : String
    , cost : List ResourceCost
    , effects : List ResourceEffect

    -- mutable
    , amount : Float
    , status : DisplayStatus
    }


getAggregateResourceEffects : List Building -> List ResourceEffect
getAggregateResourceEffects buildings =
    flatten (List.map getAggregateResourceEffectsForBuilding buildings)

getAggregateResourceEffectsForBuilding : Building -> List ResourceEffect
getAggregateResourceEffectsForBuilding building =
    List.map (\effect -> {effect | amount = effect.amount * building.amount}) building.effects

getByName : String -> List Building -> Building
getByName name list =
    case List.head (List.filter (\r -> r.name == name) list) of
        Just r ->
            r

        Nothing ->
            defaultBuilding


defaultBuilding =
    Building "default" "default.png" "default" [ ResourceCost "food" 1 ] [ ResourceEffect ResourceLimit "villagers" Additive 2 ] 0 Hidden


huts =
    Building "huts" "tipi.png" "unlockHuts" [ ResourceCost "food" 1 ] [ ResourceEffect ResourceLimit "villagers" Additive 2 ] 0 Hidden


farms =
    Building "farms" "barn.png" "unlockFarming" [ ResourceCost "lumber" 1 ] [ ResourceEffect ResourceProduction "food" Multiplicative 0.05 ] 0 Hidden


lumberMills =
    Building "lumberMills" "circular-saw.png" "unlockWoodConstruction" [ ResourceCost "lumber" 2 ] [ ResourceEffect ResourceProduction "lumber" Multiplicative 0.05 ] 0 Hidden


storerooms =
    Building "storerooms"
        "block-house.png"
        "unlockStoneConstruction"
        [ ResourceCost "lumber" 5 ]
        [ ResourceEffect ResourceLimit "food" Additive 5, ResourceEffect ResourceLimit "lumber" Additive 5, ResourceEffect ResourceLimit "stone" Additive 5 ]
        0
        Hidden


quarries =
    Building "quarries" "gold-mine.png" "unlockStoneConstruction" [ ResourceCost "lumber" 2 ] [ ResourceEffect ResourceProduction "stone" Multiplicative 0.06 ] 0 Hidden


schools =
    Building "schools" "graduate-cap.png" "unlockSchools" [ ResourceCost "lumber" 3 ] [ ResourceEffect ResourceProduction "research" Multiplicative 0.06 ] 0 Hidden


libraries =
    Building "libraries" "book-cover.png" "unlockLibraries" [ ResourceCost "lumber" 4 ] [ ResourceEffect ResourceLimit "research" Additive 5 ] 0 Hidden
