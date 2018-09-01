module Laborer exposing (Laborer, builders, farmers, foresters, hunters, idlers, laborers, miners, thinkers)

import Util exposing (..)


laborers =
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
    }


idlers =
    Laborer "idlers" "watch.png" "unlockVillagers" [] 0


farmers =
    Laborer "farmers" "farmer.png" "unlockVillagers" [ ResourceProductionMod "food" Additive 0.5 ] 0


thinkers =
    Laborer "thinkers" "think.png" "unlockVillagers" [ ResourceProductionMod "research" Additive 0.2 ] 0


foresters =
    Laborer "foresters" "hand-saw.png" "unlockWoodConstruction" [ ResourceProductionMod "lumber" Additive 0.2 ] 0


hunters =
    Laborer "hunters" "watch.png" "unlockHunting" [ ResourceProductionMod "food" Additive 0.2 ] 0


miners =
    Laborer "miners" "watch.png" "unlockStoneConstruction" [ ResourceProductionMod "stone" Additive 0.1 ] 0


builders =
    Laborer "builders" "watch.png" "unlockBuilders" [] 0
