module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser exposing (Document)
import Building exposing (..)
import Bulma exposing (..)
import FormatNumber
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, disabled, src, style)
import Html.Events exposing (..)
import Laborer exposing (..)
import PreReq exposing (..)
import Random
import Resource exposing (..)
import Tech exposing (..)
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
    , prereqs : List PreReq.PreReq
    , showAll : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Resource.resources Laborer.laborers Building.buildings Tech.techs PreReq.preReqs True
    , Cmd.none
    )



-- UPDATE


type Msg
    = HarvestResource Resource
    | BuyBuilding Building
    | SellBuilding Building


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HarvestResource resource ->
            ( { model | resources = updateResources model.resources (updateResourceAmount resource 1) }
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


buyBuilding : Model -> Building -> Model
buyBuilding model building =
    let
        multipliedCosts =
            getMultipliedCosts building.cost building.amount
    in
    if canAfford multipliedCosts model.resources then
        { model
            | resources = updateResourcesMultiple model building multipliedCosts
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
            | resources = updateResourcesMultiple model building negatedCosts
            , buildings = updateBuildings model.buildings (updateBuildingAmount building -1)
        }

    else
        model


updateResourcesMultiple : Model -> Building -> List ResourceCost -> List Resource
updateResourcesMultiple model building costs =
    List.map (updateResourceAmountFromCosts costs) model.resources


updateResourceAmountFromCosts : List ResourceCost -> Resource -> Resource
updateResourceAmountFromCosts costs resource =
    let
        resourceCost =
            getResourceCostByResource costs resource.name
    in
    { resource | amount = resource.amount - resourceCost.amount }


updateResourceAmount : Resource -> Float -> Resource
updateResourceAmount resource amount =
    { resource | amount = resource.amount + amount }


updateResources : List Resource -> Resource -> List Resource
updateResources resources updatedResource =
    let
        resourcesMinusUpdatedResource =
            List.filter (\r -> r.name /= updatedResource.name) resources
    in
    updatedResource :: resourcesMinusUpdatedResource


updateBuildingAmount : Building -> Float -> Building
updateBuildingAmount building amount =
    { building | amount = building.amount + amount }


updateBuildings : List Building -> Building -> List Building
updateBuildings buildings updatedBuilding =
    let
        buildingsMinusUpdatedBuilding =
            List.filter (\r -> r.name /= updatedBuilding.name) buildings
    in
    updatedBuilding :: buildingsMinusUpdatedBuilding


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "EricIdle"
    , body =
        [ div []
            [ stylesheet
            , hero "Eric Idle"
            , section [ class "section" ]
                [ div [ class "container" ]
                    [ div [ class "columns is-centered is-multiline" ]
                        [ div [ class "column is-narrow" ] [ resourcesTable model ]
                        , div [ class "column is-narrow" ] [ buildingsTable model ]
                        ]
                    ]
                ]
            ]
        ]
    }


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
            , th [ style "text-align" "center" ] [ text "harvest" ]
            ]
        ]


resourceRows : Model -> List (Html Msg)
resourceRows model =
    resourceHeader :: List.map resourceRow (List.filter (\r -> r.status == Shown || model.showAll) model.resources)


resourceRow : Resource -> Html Msg
resourceRow resource =
    tr []
        [ td [] [ icon resource.image ]
        , td [] [ text resource.name ]
        , td [ style "text-align" "right" ] [ text (FormatNumber.format myLocale resource.amount) ]
        , td [ style "text-align" "center" ] [ button [ class "button", onClick (HarvestResource resource) ] [ text "Harvest" ] ]
        ]


icon : String -> Html msg
icon iconSrc =
    img [ class "image is-32x32", src ("../ico/" ++ iconSrc) ] []


myLocale =
    { usLocale
        | decimals = 3
    }


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
        , span [ class "tag is-medium is-light" ] [ text (FormatNumber.format myLocale cost.amount) ]
        ]


buildingsTable : Model -> Html Msg
buildingsTable model =
    table [ class "table is-narrow" ]
        (buildingRows model)


buildingHeader : Html msg
buildingHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [ colspan 100, style "text-align" "center" ] [ text "Buildings" ]
            ]
        , tr []
            [ th [] [ text "name" ]
            , th [ style "text-align" "center" ] [ text "Price" ]
            , th [ style "text-align" "right" ] [ text "amount" ]
            , th [ style "text-align" "center" ] [ text "" ]
            ]
        ]


buildingRows : Model -> List (Html Msg)
buildingRows model =
    buildingHeader :: List.map (buildingRow model) (List.filter (\r -> r.status == Shown || model.showAll) model.buildings)


buildingRow : Model -> Building -> Html Msg
buildingRow model building =
    tr []
        [ td [] [ text building.name ]
        , td [ style "text-align" "center" ] (resourceCostsDisplay model (getMultipliedCosts building.cost building.amount))
        , td [ style "text-align" "right" ] [ text (String.fromFloat building.amount) ]
        , td [ style "text-align" "center" ] [ button [ disabled (not (canAfford (getMultipliedCosts building.cost building.amount) model.resources)), class "button", onClick (BuyBuilding building) ] [ text "Buy" ]
            , button [ disabled (building.amount <= 0), class "button", onClick (SellBuilding building) ] [ text "Sell" ]
            ]
        ]


getMultipliedCosts : List ResourceCost -> Float -> List ResourceCost
getMultipliedCosts costs n =
    List.map (\c -> { c | amount = c.amount * 1.04 ^ n }) costs


canAfford : List ResourceCost -> List Resource -> Bool
canAfford costs resources =
    List.all (checkResourceCost resources) costs


checkResourceCost : List Resource -> ResourceCost -> Bool
checkResourceCost resources resourceCost =
    let
        resourceAmount =
            getResourceAmountByName resourceCost.resource resources
    in
    resourceAmount >= resourceCost.amount


getResourceAmountByName : String -> List Resource -> Float
getResourceAmountByName name resources =
    let
        resource =
            List.head (List.filter (\r -> r.name == name) resources)
    in
    case resource of
        Just r ->
            r.amount

        Nothing ->
            0
