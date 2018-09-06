module Laborer exposing (..)

import Util exposing (..)


initialLaborers =
    [ idlers
    , farmers
    , thinkers
    , foresters
    , hunters
    , miners
    , builders
    ]


type alias Laborer =
    { name : String
    , image : String
    , prereq : String
    , effects : List ResourceEffect

    -- mutable
    , amount : Float
    , status : DisplayStatus
    }


getAggregateResourceEffects : List Laborer -> List ResourceEffect
getAggregateResourceEffects laborers =
    flatten (List.map getAggregateResourceEffectsForLaborer laborers)

getAggregateResourceEffectsForLaborer : Laborer -> List ResourceEffect
getAggregateResourceEffectsForLaborer laborer =
    List.map (\effect -> {effect | amount = effect.amount * laborer.amount}) laborer.effects

idlers =
    Laborer "idlers" "watch.png" "unlockVillagers" [] 0 Hidden


farmers =
    Laborer "farmers" "farmer.png" "unlockVillagers" [ ResourceEffect ResourceProduction "food" Additive 0.5 ] 0 Hidden


thinkers =
    Laborer "thinkers" "think.png" "unlockVillagers" [ ResourceEffect ResourceProduction "research" Additive 0.2 ] 0 Hidden


foresters =
    Laborer "foresters" "hand-saw.png" "unlockWoodConstruction" [ ResourceEffect ResourceProduction "lumber" Additive 0.2 ] 0 Hidden


hunters =
    Laborer "hunters" "watch.png" "unlockHunting" [ ResourceEffect ResourceProduction "food" Additive 0.2 ] 0 Hidden


miners =
    Laborer "miners" "watch.png" "unlockStoneConstruction" [ ResourceEffect ResourceProduction "stone" Additive 0.1 ] 0 Hidden


builders =
    Laborer "builders" "watch.png" "unlockBuilders" [] 0 Hidden
