module Util exposing (..)

import Debug

flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list


getByName : String -> List { a | name : String } -> { a | name : String }
getByName name list =
    case List.head (List.filter (hasName name) list) of
        Just item ->
            item

        Nothing ->
            Debug.todo "TODO"

hasName : String -> {a | name : String} -> Bool
hasName desiredName {name} =
    name == desiredName

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


type AggregationType
    = Additive
    | Multiplicative

type SubType
    = ResourceProduction
    | ResourceLimit


type alias ResourceEffect =
    { subType : SubType
    , resourceName : String
    , aggregationType : AggregationType
    , amount : Float
    }
