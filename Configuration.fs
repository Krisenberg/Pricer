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
    let drift = if marketData.ContainsKey "parameters::drift" then float marketData.[ "parameters::drift" ] else 5.0    
    drift

let assetValues (assetName: AssetKey) (assetsData: AssetsData) =
    let key = sprintf "asset::%s" assetName

    let spot = if assetsData.ContainsKey key then float assetsData[ key ].Spot else 100.0
    let vol = if assetsData.ContainsKey key then float assetsData[ key ].Vol else 10.0
    (spot, vol)