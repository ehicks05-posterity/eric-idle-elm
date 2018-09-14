module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (Document)
import Building exposing (..)
import Bulma exposing (..)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (alt, class, colspan, disabled, height, href, src, style, value, width)
import Html.Attributes.Aria exposing (ariaExpanded, ariaHidden, ariaLabel, role)
import Html.Events exposing (..)
import Laborer exposing (..)
import Maybe exposing (Maybe)
import PreReq exposing (..)
import Random
import Resource exposing (..)
import Task
import Tech exposing (..)
import Time exposing (..)
import Util exposing (..)



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { resources : List Resource.Resource
    , laborers : List Laborer.Laborer
    , buildings : List Building.Building
    , technologies : List Tech.Tech
    , preReqs : List PreReq.PreReq
    , showAll : Bool
    , isDarkly : Bool
    , waitingForAVillager : Bool
    , nextVillagerArrival : Time.Posix
    , infoEntityName : String
    , infoEntityType : String
    , currentTime : Time.Posix
    , zone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Resource.resources Laborer.initialLaborers Building.initialBuildings Tech.techs PreReq.preReqs True False False (Time.millisToPosix 0) "" "" (Time.millisToPosix 0) Time.utc
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | HarvestResource Resource
    | BuyBuilding Building
    | SellBuilding Building
    | AddLaborer Laborer
    | RemoveLaborer Laborer
    | ToggleShowAll
    | ToggleDarkly
    | ShowInfo String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posixTime ->
            ( doTick model posixTime
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        HarvestResource resource ->
            ( { model | resources = updateResources model.resources (updateResourceAmount model resource 1) }
            , Cmd.none
            )

        BuyBuilding building ->
            ( buyBuilding model building
            , Cmd.none
            )

        SellBuilding building ->
            ( sellBuilding model building
            , Cmd.none
            )

        AddLaborer laborer ->
            ( addLaborer model laborer
            , Cmd.none
            )

        RemoveLaborer laborer ->
            ( removeLaborer model laborer
            , Cmd.none
            )

        ToggleShowAll ->
            ( { model | showAll = not model.showAll }
            , Cmd.none
            )

        ToggleDarkly ->
            ( { model | isDarkly = not model.isDarkly }
            , Cmd.none
            )

        ShowInfo infoEntityName infoEntityType ->
            ( { model | infoEntityName = infoEntityName, infoEntityType = infoEntityType }
            , Cmd.none
            )


doTick : Model -> Time.Posix -> Model
doTick model newPosixTime =
    let
        newMillisTime =
            Time.posixToMillis newPosixTime

        newPreReqs =
            PreReq.updatePreReqs model.preReqs

        didAVillagerJustArrive =
            didAVillagerArrive model newPosixTime

        newResources =
            resourceTick model didAVillagerJustArrive

        newWaitingForAVillager =
            not didAVillagerJustArrive && isThereRoomForAVillager model

        newNextVillagerArrival =
            if didAVillagerJustArrive then
                Debug.log "case0" (Time.millisToPosix 0)

            else if model.waitingForAVillager == False && isThereRoomForAVillager model then
                Debug.log "case1" (Time.millisToPosix (newMillisTime + 5000))

            else
                Debug.log "case2" model.nextVillagerArrival
    in
    { model | currentTime = newPosixTime, preReqs = newPreReqs, resources = newResources, waitingForAVillager = newWaitingForAVillager, nextVillagerArrival = newNextVillagerArrival }


resourceTick : Model -> Bool -> List Resource
resourceTick model didAVillagerJustArrive =
    let
        resources =
            model.resources

        villagersToAdd =
            if didAVillagerJustArrive then
                1

            else
                0
    in
    updateResources resources (updateResourceAmount model (Util.getByName "villagers" resources) villagersToAdd)


isThereRoomForAVillager : Model -> Bool
isThereRoomForAVillager model =
    let
        villagers =
            Util.getByName "villagers" model.resources

        villagersAmount =
            villagers.amount

        villagersLimit =
            getResourceLimit model villagers
    in
    villagersAmount < villagersLimit


didAVillagerArrive : Model -> Time.Posix -> Bool
didAVillagerArrive model posixTime =
    if isThereRoomForAVillager model && model.waitingForAVillager && Time.posixToMillis model.nextVillagerArrival <= Time.posixToMillis posixTime then
        True

    else
        False


handleVillagerArrival : Model -> Time.Posix -> List Resource
handleVillagerArrival model posixTime =
    let
        villagers =
            Util.getByName "villagers" model.resources
    in
    updateResources model.resources (updateResourceAmount model villagers 1)


getAllResourceEffects : Model -> List ResourceEffect
getAllResourceEffects model =
    Laborer.getAggregateResourceEffects model.laborers ++ Building.getAggregateResourceEffects model.buildings


addLaborer : Model -> Laborer -> Model
addLaborer model laborer =
    let
        newIdler =
            updateLaborerAmount (Util.getByName "idlers" model.laborers) -1

        newLaborer =
            updateLaborerAmount laborer 1

        laborerName =
            laborer.name

        laborers =
            model.laborers

        newLaborers =
            updateLaborers newIdler model.laborers
                |> updateLaborers newLaborer
    in
    if haveAnIdler model.laborers then
        { model | laborers = newLaborers }

    else
        model


updateLaborerAmount : Laborer -> Float -> Laborer
updateLaborerAmount laborer amount =
    { laborer | amount = laborer.amount + amount }


updateLaborers : Laborer -> List Laborer -> List Laborer
updateLaborers updatedLaborer laborers =
    let
        laborersMinusUpdatedLaborer =
            List.filter (\r -> r.name /= updatedLaborer.name) laborers
    in
    List.sortBy .index (updatedLaborer :: laborersMinusUpdatedLaborer)


removeLaborer : Model -> Laborer -> Model
removeLaborer model laborer =
    let
        newIdler =
            updateLaborerAmount (Util.getByName "idlers" model.laborers) 1

        newLaborer =
            updateLaborerAmount laborer -1

        laborerName =
            laborer.name

        laborers =
            model.laborers

        newLaborers =
            updateLaborers newIdler model.laborers
                |> updateLaborers newLaborer
    in
    if laborer.amount > 0 then
        { model | laborers = newLaborers }

    else
        model


buyBuilding : Model -> Building -> Model
buyBuilding model building =
    let
        multipliedCosts =
            getMultipliedCosts building.cost building.amount
    in
    if canAffordResourceCosts multipliedCosts model.resources then
        { model
            | resources = updateResourcesMultiple model multipliedCosts
            , buildings = updateBuildings model.buildings (updateBuildingAmount building 1)
        }

    else
        model


sellBuilding : Model -> Building -> Model
sellBuilding model building =
    let
        multipliedCosts =
            getMultipliedCosts building.cost (building.amount - 1)

        negatedCosts =
            List.map (\rc -> { rc | amount = negate rc.amount }) multipliedCosts
    in
    if building.amount > 0 then
        { model
            | resources = updateResourcesMultiple model negatedCosts
            , buildings = updateBuildings model.buildings (updateBuildingAmount building -1)
        }

    else
        model


updateResourcesMultiple : Model -> List ResourceCost -> List Resource
updateResourcesMultiple model costs =
    List.map (updateResourceAmountFromCosts model costs) model.resources


updateResourceAmountFromCosts : Model -> List ResourceCost -> Resource -> Resource
updateResourceAmountFromCosts model costs resource =
    let
        resourceCost =
            getResourceCostByResource costs resource.name
    in
    updateResourceAmount model resource (negate resourceCost.amount)


getResourceLimit : Model -> Resource -> Float
getResourceLimit model resource =
    let
        mods =
            List.filter (\re -> re.resourceName == resource.name) (getAllResourceEffects model)

        limitMods =
            List.filter (\re -> re.subType == ResourceLimit) mods

        ( additiveMods, multiplicativeMods ) =
            List.partition (\re -> re.aggregationType == Additive) limitMods

        additiveTotal =
            List.foldr (+) 0 (List.map (\re -> re.amount) additiveMods)

        multiplicativeTotal =
            List.foldr (+) 0 (List.map (\re -> re.amount) multiplicativeMods)
    in
    (resource.baseLimit + additiveTotal) * (1 + multiplicativeTotal)


getResourceProductionRate : Model -> Resource -> Float
getResourceProductionRate model resource =
    let
        mods =
            List.filter (\re -> re.resourceName == resource.name) (getAllResourceEffects model)

        productionMods =
            List.filter (\re -> re.subType == ResourceProduction) mods

        ( additiveMods, multiplicativeMods ) =
            List.partition (\re -> re.aggregationType == Additive) productionMods

        additiveTotal =
            List.foldr (+) 0 (List.map (\re -> re.amount) additiveMods)

        multiplicativeTotal =
            List.foldr (+) 0 (List.map (\re -> re.amount) multiplicativeMods)
    in
    additiveTotal * (1 + multiplicativeTotal)



{-
   enforce resource caps
-}


updateResourceAmount : Model -> Resource -> Float -> Resource
updateResourceAmount model resource amount =
    let
        newAmount =
            min (resource.amount + amount) (getResourceLimit model resource)
    in
    { resource | amount = newAmount }


updateResources : List Resource -> Resource -> List Resource
updateResources resources updatedResource =
    let
        resourcesMinusUpdatedResource =
            List.filter (\r -> r.name /= updatedResource.name) resources
    in
    List.sortBy .index (updatedResource :: resourcesMinusUpdatedResource)


updateBuildingAmount : Building -> Float -> Building
updateBuildingAmount building amount =
    { building | amount = building.amount + amount }


updateBuildings : List Building -> Building -> List Building
updateBuildings buildings updatedBuilding =
    let
        buildingsMinusUpdatedBuilding =
            List.filter (\r -> r.name /= updatedBuilding.name) buildings
    in
    List.sortBy .index (updatedBuilding :: buildingsMinusUpdatedBuilding)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 16.67 Tick



-- VIEW


view : Model -> Document Msg
view model =
    { title = "EricIdle"
    , body =
        [ div []
            [ stylesheet model.isDarkly
            , navBar model
            , div []
                [ text
                    (if model.waitingForAVillager then
                        "Waiting for a villager..." ++ "(" ++ String.fromFloat (Util.timeDifference model.currentTime model.nextVillagerArrival) ++ " seconds)"

                     else
                        "Not waiting for a villager..."
                    )
                ]
            , section [ class "section" ]
                --                [ div [ class "container" ]
                [ div [ class "columns is-centered" ]
                    [ div [ class "column" ]
                        [ div [ class "columns is-centered is-multiline" ]
                            [ div [ class "column is-narrow" ] [ resourcesTable model ]
                            , div [ class "column is-narrow" ] [ buildingsTable model ]
                            , div [ class "column is-narrow" ] [ laborersTable model ]
                            ]
                        ]
                    , div [ class "column is-one-fifth" ]
                        [ infoDisplay model
                        ]
                    ]
                ]

            --                ]
            ]
        ]
    }


infoDisplay : Model -> Html Msg
infoDisplay model =
    case model.infoEntityType of
        "building" ->
            infoBuildingDisplay model

        "laborer" ->
            infoLaborerDisplay model

        _ ->
            div [] []


infoBuildingDisplay : Model -> Html Msg
infoBuildingDisplay model =
    let
        infoBuilding =
            case model.infoEntityName of
                "" ->
                    Maybe.Nothing

                _ ->
                    Maybe.Just (Util.getByName model.infoEntityName model.buildings)
    in
    case infoBuilding of
        Just building ->
            div []
                [ p [ class "title is-4" ] [ text building.name ]
                , p [ class "subtitle is-6" ] [ text "flavor text" ]
                , img [ class "image is-64x64", src (iconUrl building.image) ] []

                --                , p [] [ text ("Amount: " ++ String.fromFloat building.amount) ]
                , div [] (resourceCostsDisplay model (getMultipliedCosts building.cost building.amount))
                , resourceEffectsDisplay building.effects
                ]

        Nothing ->
            div [] []


infoLaborerDisplay : Model -> Html Msg
infoLaborerDisplay model =
    let
        infoLaborer =
            case model.infoEntityName of
                "" ->
                    Maybe.Nothing

                _ ->
                    Maybe.Just (Util.getByName model.infoEntityName model.laborers)
    in
    case infoLaborer of
        Just laborer ->
            div []
                [ p [ class "title" ] [ text laborer.name ]
                , p [ class "subtitle" ] [ text "flavor text" ]
                , img [ class "image is-64x64", src (iconUrl laborer.image) ] []

                --                , p [] [ text ("Amount: " ++ String.fromFloat laborer.amount) ]
                , div [] (resourceCostsDisplay model [ ResourceCost "villagers" 1 ])
                , resourceEffectsDisplay laborer.effects
                ]

        Nothing ->
            div [] []


navBarBurger =
    a [ role "button", class "navbar-burger", ariaLabel "menu", ariaExpanded "false" ]
        [ span [ ariaHidden True ] []
        , span [ ariaHidden True ] []
        , span [ ariaHidden True ] []
        ]


navBarBrand =
    div [ class "navbar-brand" ]
        [ a [ class "navbar-item", href "/" ]
            --        [img [src "https://bulma.io/images/bulma-logo.png", alt "Bulma: a modern CSS framework based on Flexbox", width 112, height 28] [] ]
            [ text "Eric Idle" ]
        , navBarBurger
        ]


navBarMenu model =
    div [ class "navbar-menu" ]
        [ div [ class "navbar-start" ]
            [ div [ class "navbar-item" ]
                [ div [ class "buttons" ] [ debugButton, darklyButton model.isDarkly ] ]
            ]
        , div [ class "navbar-end" ]
            [ div [ class "navbar-item" ]
                [ div [ class "" ] [ text (formatTime model.zone model.currentTime) ] ]
            ]
        ]


debugButton =
    button [ class "button", onClick ToggleShowAll ] [ text "Debug" ]


darklyButton isDarkly =
    button [ class "button", onClick ToggleDarkly ]
        [ text
            (if isDarkly then
                "Normal"

             else
                "Darkly"
            )
        ]


navBar model =
    nav [ class "navbar", role "navigation", ariaLabel "main navigation" ]
        [ navBarBrand
        , navBarMenu model
        ]


resourcesTable : Model -> Html Msg
resourcesTable model =
    table [ class "table is-narrow" ]
        (resourceRows model)


resourceHeader : Html msg
resourceHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [ colspan 100, style "text-align" "center" ] [ text "Resources" ]
            ]
        , tr []
            [ th [] [ text "" ]
            , th [] [ text "name" ]
            , th [ style "text-align" "right" ] [ text "amount" ]
            , th [ style "text-align" "right" ] [ text "rate" ]
            , th [ style "text-align" "center" ] [ text "" ]
            , th [ style "text-align" "center" ] [ text "" ]
            ]
        ]


resourceRows : Model -> List (Html Msg)
resourceRows model =
    resourceHeader :: List.map (resourceRow model) (List.filter (\r -> r.status == Shown || model.showAll) model.resources)


resourceRow : Model -> Resource -> Html Msg
resourceRow model resource =
    tr []
        [ td [] [ icon resource.image ]
        , td [] [ text resource.name ]
        , td [ style "text-align" "right" ] [ text (myFormat resource.amount ++ " / " ++ myFormat (getResourceLimit model resource)) ]
        , td [ style "text-align" "right" ] [ text (myFormat (getResourceProductionRate model resource)) ]
        , td [ style "text-align" "center" ] [ harvestButton resource ]
        , td [style "width" "100px"] [resourceProgressBar model resource]
        ]


resourceProgressBar model resource =
    let
        seconds =
            (Util.timeDifference model.currentTime model.nextVillagerArrival)

        percentString = String.fromFloat (100 * (5 - seconds) / 5)
    in
    case (resource.name, model.waitingForAVillager) of
        ("villagers", True) ->
            progress [ class "progress is-primary is-small", value percentString, Html.Attributes.max "100" ] [ text percentString ]

        _ ->
            text ""


harvestButton : Resource -> Html Msg
harvestButton resource =
    if resource.name == "food" then
        button [ class "button", onClick (HarvestResource resource) ] [ text "+" ]

    else
        text ""


icon : String -> Html msg
icon iconSrc =
    img [ class "image is-32x32", src (iconUrl iconSrc) ] []


iconUrl : String -> String
iconUrl iconSrc =
    "../ico/" ++ iconSrc


myFormat n =
    FormatNumber.format usLocale n


resourceCostsDisplay : Model -> List ResourceCost -> List (Html msg)
resourceCostsDisplay model costs =
    List.map (resourceCostDisplay model) costs


resourceCostDisplay : Model -> ResourceCost -> Html msg
resourceCostDisplay model cost =
    let
        resource =
            Resource.getByName cost.resource model.resources
    in
    div [ class "tags has-addons" ]
        [ span [ class "tag is-light", style "padding" "0px" ] [ img [ style "border-radius" "4px 0 0 4px", class "image is-32x32", src ("../ico/" ++ resource.image) ] [] ]
        , span [ class "tag is-medium" ] [ text (myFormat cost.amount) ]
        ]


resourceEffectsDisplay : List ResourceEffect -> Html msg
resourceEffectsDisplay effects =
    table [ class "table is-narrow" ] (List.map resourceEffectDisplay effects)


resourceEffectDisplay : ResourceEffect -> Html msg
resourceEffectDisplay effect =
    let
        amount =
            case effect.aggregationType of
                Additive ->
                    effect.amount

                Multiplicative ->
                    effect.amount * 100

        displayAmount =
            String.fromFloat amount

        subType =
            case effect.subType of
                ResourceProduction ->
                    "production"

                ResourceLimit ->
                    "limit"

        aggregationType =
            case effect.aggregationType of
                Additive ->
                    ""

                Multiplicative ->
                    "%"
    in
    tr [ class "" ]
        [ td [] [ text ("Increase " ++ effect.resourceName ++ " " ++ subType ++ " by " ++ displayAmount ++ aggregationType) ]
        ]


buildingsTable : Model -> Html Msg
buildingsTable model =
    ul [ class "table is-narrow" ]
        (buildingRows model)


buildingHeader : Html msg
buildingHeader =
    thead [ class "thead" ]
        [ tr [] [ th [ colspan 100, style "text-align" "center" ] [ text "Buildings" ] ] ]


buildingRows : Model -> List (Html Msg)
buildingRows model =
    buildingHeader :: List.map (buildingRow model) (List.filter (\r -> r.status == Shown || model.showAll) model.buildings)


buildingRow : Model -> Building -> Html Msg
buildingRow model building =
    tr [ onMouseOver (ShowInfo building.name "building") ]
        [ td []
            [ div [ class "buttons has-addons" ]
                [ button
                    [ class "button is-expanded"
                    , onClick (BuyBuilding building)
                    , disabled (not (canAffordResourceCosts (getMultipliedCosts building.cost building.amount) model.resources))
                    ]
                    [ text building.name ]
                , button [ class "button is-static" ] [ text (String.fromFloat building.amount) ]
                , button [ class "button", onClick (SellBuilding building), disabled (building.amount <= 0) ] [ text "Sell" ]
                ]
            ]
        ]


getMultipliedCosts : List ResourceCost -> Float -> List ResourceCost
getMultipliedCosts costs n =
    List.map (\c -> { c | amount = c.amount * getQuantityMultiplier n }) costs


getQuantityMultiplier : Float -> Float
getQuantityMultiplier quantity =
    1.04 ^ quantity


canAffordResourceCosts : List ResourceCost -> List Resource -> Bool
canAffordResourceCosts costs resources =
    List.all (canAffordResourceCost resources) costs


canAffordResourceCost : List Resource -> ResourceCost -> Bool
canAffordResourceCost resources resourceCost =
    let
        resourceAmount =
            (Resource.getByName resourceCost.resource resources).amount
    in
    resourceAmount >= resourceCost.amount


laborersTable : Model -> Html Msg
laborersTable model =
    table [ class "table is-narrow" ]
        (laborerRows model)


laborerHeader : Html msg
laborerHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [ colspan 100, style "text-align" "center" ] [ text "Laborers" ]
            ]
        ]


laborerRows : Model -> List (Html Msg)
laborerRows model =
    laborerHeader :: List.map (laborerRow model) (List.filter (\r -> r.status == Shown || model.showAll) model.laborers)


laborerRow : Model -> Laborer -> Html Msg
laborerRow model laborer =
    tr [ onMouseOver (ShowInfo laborer.name "laborer") ]
        [ td []
            [ div [ class "buttons has-addons" ]
                [ button
                    [ class "button is-expanded"
                    , onClick (AddLaborer laborer)
                    , disabled (not (haveAnIdler model.laborers))
                    ]
                    [ text laborer.name ]
                , button [ class "button is-static" ] [ text (String.fromFloat laborer.amount) ]
                , button [ class "button", onClick (RemoveLaborer laborer), disabled (laborer.amount <= 0) ] [ text "-" ]
                ]
            ]
        ]


haveAnIdler : List Laborer -> Bool
haveAnIdler laborers =
    (Util.getByName "idlers" laborers).amount > 0
