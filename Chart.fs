module Chart
open Radzen.Blazor
open System
open Money
open Trades
open EuropeanOption


type ChartItem =
    { 
      XValue : float
      YValue : float
    }

type Series =
  {
    Values : ChartItem []
    SeriesName : string
    Line : LineType
    Color : string
    Marker : MarkerType
    Smooth : bool
    ShowLabels : bool
  }
  static member Default = 
    {
      Values = [||];
      SeriesName = "Series1";
      Line = LineType.Solid 
      Color = "Black"
      Marker = MarkerType.None
      Smooth = false
      ShowLabels = false
    }

type itemsXaxis =
  | SpotPrice
  | StrikePrice
  | Volatility
  | Drift
  | Time

type itemsYaxis =
  | Value
  | Delta

type ChartData =
  {
    Series     : Series[]
    Title      : string
    ItemX      : itemsXaxis
    ItemY      : itemsYaxis
    Trades     : TradeID array
    ScopeX     : float * float
    Data       : Configuration.Configuration
    MarketData : Configuration.MarketData
  }
  static member Default =
      { Series = [||]
        Title = "Default Trade"
        ItemX = SpotPrice
        ItemY = Value
        Trades = [||]
        ScopeX = (0.01, 300.)
        Data = Map.empty
        MarketData = Map.empty
      }

let parseItemXaxis (item : string) =
  match item with
  | "Spot Price" -> Some SpotPrice
  | "Strike Price" -> Some StrikePrice
  | "Volatility" -> Some Volatility
  | "Drift" -> Some Drift
  | _ -> Some Time

let parseItemYaxis (item : string) =
  match item with
  | "Value" -> Some Value
  | "Delta" -> Some Delta
  | _ -> None

let itemXaxisToString (item : itemsXaxis) =
  match item with
  | SpotPrice -> "Spot Price"
  | StrikePrice -> "Strike Price"
  | Volatility -> "Volatility"
  | Drift -> "Drift"
  | Time -> "Time"

let makeEuropeanOptionsChart (itemXaxis, itemYaxis, eoTrade, eoTradeID, scopeX, data, marketData, assetsData) : ChartData =
    let scopeXlow, scopeXhigh = scopeX

    let makeSeries(eo : EuropeanOptionRecord) : Series =
      let name = eo.TradeName

      let valuationModel = 
        let inputs = 
          {
            Trade = eo
            Data = data
            MarketData = marketData
            AssetsData = assetsData
          }
        EuropeanOptionValuationModel(inputs)

      let getMoneyValue (money : Money) = money.Value
      // let convertIntToDateTime (months : int) : DateTime = (DateTime.Now.AddMonths(months))
      let convertIntToDateTime (days : float) : DateTime = (DateTime.Now.AddDays(days))

      let calcFunc =
        match itemYaxis with
          | Value -> valuationModel.Simulate >> fst >> getMoneyValue
          | Delta -> valuationModel.Simulate >> snd

      let args xAxisArg =
        let drift = Configuration.marketDrift marketData
        let (assetSpot, assetVol) = Configuration.assetValues eo.Asset assetsData
        match itemXaxis with
          | SpotPrice -> (xAxisArg, eo.Strike, drift, assetVol, eo.Expiry)
          | StrikePrice -> (assetSpot, xAxisArg, drift, assetVol, eo.Expiry)
          | Drift -> (assetSpot, eo.Strike, xAxisArg, assetVol, eo.Expiry)
          | Volatility -> (assetSpot, eo.Strike, drift, xAxisArg, eo.Expiry)
          | Time -> (assetSpot, eo.Strike, drift, assetVol, (convertIntToDateTime (xAxisArg)))

      //step 1: have a sequence of values (x,y)
      let series =
        let step = (scopeXhigh - scopeXlow) / 200.
        match itemXaxis with
        | Time -> seq {
            for i in scopeXlow .. step .. scopeXhigh do
              yield float i, args (30.0 * (float i)) |> calcFunc
              // yield float i, ((float i) * 100.0)
          }
        | _ -> seq {
            for i in scopeXlow .. step .. scopeXhigh do
              yield float i, args (float i) |> calcFunc
          }
        // seq {
        //     for i in scopeXlow .. step .. scopeXhigh do
        //       yield float i, args (float i) |> calcFunc
        //   }

      //step 2: map those to ChartItem
      let values =
            series
            |> Seq.map (fun (x,y) -> {XValue = x; YValue = y})
            |> Array.ofSeq

      //step 3: customize the series, change color, name etc
      { Series.Default with
            Values = values
            SeriesName = name
      }

    let title = "Chart: " + (string itemYaxis) + "(" + itemXaxisToString(itemXaxis) + ")"
    //step 4: add or replace series on existing chart
    { ChartData.Default with 
        Series = Array.map makeSeries eoTrade
        Title = title
        ItemX = itemXaxis
        ItemY = itemYaxis
        Trades = eoTradeID
        ScopeX = scopeX
        Data = data
        MarketData = marketData
    }