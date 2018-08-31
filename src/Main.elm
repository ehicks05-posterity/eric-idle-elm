module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Bulma exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, colspan, disabled, src, style)
import Html.Events exposing (..)
import Random
import Util exposing (..)
import Laborer exposing (..)
import Building exposing (..)
import Tech exposing (..)
import Resource exposing (..)
import PreReq exposing (..)

-- MAIN


main =
    Browser.element
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
    = HarvestResource String
    | BuyBuilding String
    | SellBuilding String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HarvestResource resourceName ->
            ( {model | resources = harvestResource model.resources resourceName}
            , Cmd.none
            )

        BuyBuilding string ->
            ( model
            , Cmd.none
            )

        SellBuilding string ->
            ( model
            , Cmd.none
            )

getByName : List a -> String -> a
getByName list name =
    case list of
        {}
        result =

buyBuilding : Model -> String -> Model
buyBuilding model buildingName =
    let
        building =


harvestResource : List Resource -> String -> List Resource
harvestResource resources resourceName =
    let
        (matchingResources, resourcesMinusMatchingResource) = List.partition (\r -> r.name == resourceName) resources

        maybeResource = List.head matchingResources

--        updatedResource = {maybeResource | amount = maybeResource.amount + 1}
    in
        case maybeResource of
            Just resource ->
                {resource | amount = resource.amount + 1} :: resourcesMinusMatchingResource
            Nothing ->
                resources


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ stylesheet
        , hero "Eric Idle"
        , section [ class "section" ]
            [ div [ class "container" ]
                [ resourcesTable model
                , buildingsTable model
                ]
            ]
        ]


resourcesTable : Model -> Html Msg
resourcesTable model =
    table [ class "table" ]
        (resourceRows model)


resourceHeader : Html msg
resourceHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [ colspan 100, style "text-align" "center" ] [ text "Resources" ]
            ]
        , tr []
            [ th [] [ text "name" ]
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
        [ td [] [ text resource.name ]
        , td [ style "text-align" "right" ] [ text (String.fromFloat resource.amount) ]
        , td [ style "text-align" "center" ] [ button [ class "button", onClick (HarvestResource resource.name) ] [ text "Harvest" ] ]
        ]


buildingsTable : Model -> Html Msg
buildingsTable model =
    table [ class "table" ]
        (buildingRows model)


buildingHeader : Html msg
buildingHeader =
    thead [ class "thead" ]
        [ tr []
            [ th [ colspan 100, style "text-align" "center" ] [ text "Buildings" ]
            ]
        , tr []
            [ th [] [ text "name" ]
            , th [ style "text-align" "right" ] [ text "amount" ]
            , th [ style "text-align" "center" ] [ text "Buy" ]
            , th [ style "text-align" "center" ] [ text "Sell" ]
            ]
        ]


buildingRows : Model -> List (Html Msg)
buildingRows model =
    buildingHeader :: List.map (buildingRow model) (List.filter (\r -> r.status == Shown || model.showAll) model.buildings)


buildingRow : Model -> Building -> Html Msg
buildingRow model building =
    tr []
        [ td [] [ text building.name ]
        , td [ style "text-align" "right" ] [ text (String.fromFloat building.amount) ]
        , td [ style "text-align" "center" ] [ button [ disabled (not (canAfford building.cost model.resources)), class "button", onClick (BuyBuilding building.name) ] [ text "Buy" ] ]
        , td [ style "text-align" "center" ] [ button [ disabled (building.amount <= 0), class "button", onClick (SellBuilding building.name) ] [ text "Sell" ] ]
        ]


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
