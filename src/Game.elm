module Game exposing (..)

import Building exposing (..)
import Dict exposing (Dict)
import Laborer exposing (..)
import Resource exposing (..)
import Tech exposing (..)
import Util exposing (..)



-- GameData
--type alias GameData =
--    { resources : List Resource.Resource
--    , laborers : List Laborer.Laborer
--    , buildings : List Building.Building
--    , technologies : List Tech.Tech
--    , prereqs : List PreReq
--    }
--
--
--getGameData : GameData
--getGameData =
--    GameData Resource.resources Laborer.laborers Building.buildings Tech.techs preReqs
