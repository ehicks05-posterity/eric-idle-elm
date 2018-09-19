module Game exposing (Entity, EntityType(..), defaultEntity)

import Util exposing (..)


type EntityType
    = Resource
    | Building
    | Laborer
    | PreReq
    | Tech


type alias Entity =
    { etype : EntityType
    , index : Int
    , name : String
    , image : String
    , prereq : String
    , baseLimit : Float
    , costs : List ResourceCost
    , effects : List ResourceEffect

    -- mutable
    , discovered : Bool -- for Techs
    , unlocked : LockStatus -- for PreReqs
    , displayStatus : DisplayStatus
    , amount : Float
    }


defaultEntity =
    Entity Resource 0 "" "" "" 0.0 [] [] False Locked Hidden 0.0
