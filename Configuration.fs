module Configuration
open System

type ConfigKey = string
type ConfigValue = string
type ConfigEntry = { Key : ConfigKey; Value : ConfigValue }
type ConfigCategory = { Category : string; Config : ConfigEntry[]  }
type JsonConfig = ConfigCategory []
type ConfigurationRecord = 
    {
        Key : ConfigKey
        Value : ConfigValue
    }

type AssetKey = string
type AssetSpot = string
type AssetVol = string
type AssetValue = { Spot : AssetSpot; Vol : AssetVol }
type AssetEntry = { Key : AssetKey; Spot : AssetSpot; Vol : AssetVol }
type AssetCategory = { Category : string; Assets : AssetEntry[]  }
type JsonAssets = AssetCategory []
type AssetRecord = 
    {
        Key : AssetKey
        Spot : AssetSpot
        Vol : AssetVol
    }

type Configuration = Map<ConfigKey, ConfigValue>
type MarketData = Map<ConfigKey, ConfigValue>
type AssetsData = Map<AssetKey, AssetValue>

let marketDrift (marketData: MarketData) = 
    let driftString =
        marketData.TryFind "parameters::drift"
        |> Option.defaultValue "5.0"
    driftString
    |> Double.TryParse
    |> Utils.ofBool
    |> Option.defaultValue 5.0

let assetValues (assetName: AssetKey) (assetsData: AssetsData) =
    let key = sprintf "parameters::%s" assetName
    let assetValue =
        assetsData.TryFind key
        |> Option.defaultValue { Spot = "100.0"; Vol = "10.0" }
    let spot = 
        assetValue.Spot
        |> Double.TryParse
        |> Utils.ofBool
        |> Option.defaultValue 100.0
    let vol = 
        assetValue.Vol
        |> Double.TryParse
        |> Utils.ofBool
        |> Option.defaultValue 10.0
    (spot, vol)