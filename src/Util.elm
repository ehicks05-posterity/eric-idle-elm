module Util exposing (AggregationType(..), DisplayStatus(..), LockStatus(..), ResourceCost, ResourceEffect, SubType(..), flatten, formatTime, getByName, getResourceCostByResource, hasName, removeExtension, timeDifference)

import Debug
import Time


flatten : List (List a) -> List a
flatten list =
    List.foldr (++) [] list


removeExtension : String -> String
removeExtension s =
    let
        indexes =
            String.indexes "." s

        lastIndex =
            List.head (List.reverse indexes)
    in
    case lastIndex of
        Just index ->
            String.slice 0 index s

        Nothing ->
            s


getByName : String -> List { a | name : String } -> { a | name : String }
getByName name list =
    case List.head (List.filter (hasName name) list) of
        Just item ->
            item

        Nothing ->
            Debug.todo "TODO"


hasName : String -> { a | name : String } -> Bool
hasName desiredName { name } =
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



-- TIME


formatTime zone time =
    let
        militaryHour =
            Time.toHour zone time

        amPm =
            if militaryHour < 12 then
                "AM"

            else
                "PM"

        hourNonMilitary =
            if militaryHour < 12 then
                militaryHour

            else
                militaryHour - 12

        hour =
            String.fromInt hourNonMilitary

        minute =
            formatTimeComponent (Time.toMinute zone time)

        second =
            formatTimeComponent (Time.toSecond zone time)
    in
    hour ++ ":" ++ minute ++ ":" ++ second ++ " " ++ amPm


timeDifference posix1 posix2 =
    let
        millis =
            Time.posixToMillis posix2 - Time.posixToMillis posix1

        seconds =
            toFloat millis / 1000
    in
    seconds



-- adds leading zero if the time component would otherwise be one digit.


formatTimeComponent int =
    let
        leadingZero =
            if int < 10 then
                "0"

            else
                ""
    in
    leadingZero ++ String.fromInt int
