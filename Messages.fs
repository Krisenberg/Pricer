module Messages

open Configuration
open Model
open Trades

type TradeChangeMsg =
    | NewName of TradeID * string
    | NewAsset of TradeID * string
    | NewPrincipal of TradeID * string
    | NewCurrency of TradeID * string
    | NewExpiry of TradeID * string
    | NewSpotPrice of TradeID * string
    | NewStrike of TradeID * string
    | NewDrift of TradeID * string
    | NewVolatility of TradeID * string
    | NewStrikeType of TradeID * string
    | NewValuationMethod of TradeID * string
    | NewOptionType of TradeID * string

type EOChartChangeMsg =
    | NewTrade of TradeID
    | NewItemXaxis of string
    | NewItemYaxis of string
    | NewScopeX of string

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | AddPayment
    | AddEuropeanOption
    | AddAsianOption
    | RemoveTrade of TradeID
    | TradeChange of TradeChangeMsg
    | DrawChart
    | RecalculateAllPayments
    | RecalculateAllEO
    | RecalculateAllAO
    | RecalculateTrade of TradeID
    | EOChartChange of EOChartChangeMsg
    | LoadData
    | GotConfig of JsonConfig
    | ConfigChange of string * string
    | GotMarketData of JsonConfig
    | MarketDataChange of string * string
    | GotAssetsData of JsonAssets
    | AssetsDataChange of string * string * string
    | Warning of string
    | Error of exn
    | ClearError
