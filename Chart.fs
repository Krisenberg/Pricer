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
        Title = "Title"
        ItemX = SpotPrice
        ItemY = Value
        Trades = [||]
        ScopeX = (0., 300.)
        Data = Map.empty
        MarketData = Map.empty
      }

//TODO: remove after we add non-dummy chart
//how to construct a chart:

let parseItemXaxis (item : string) =
  match item with
  | "Spot Price" -> Some SpotPrice
  | "Strike Price" -> Some StrikePrice
  | "Volatility" -> Some Volatility
  | "Drift" -> Some Drift
  | _ -> None

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

let makeEuropeanOptionsChart (itemXaxis, itemYaxis, eoTrade, eoTradeID, scopeX, data, marketData) : ChartData =
    let scopeXlow, scopeXhigh = scopeX

    let makeSeries(eoTrade : EuropeanOptionRecord) : Series =
      let name = eoTrade.TradeName

      let valuationModel = 
        let inputs = 
          {
            Trade = eoTrade
            Data = data
            MarketData = marketData
          }
        EuropeanOptionValuationModel(inputs)

      let getMoneyValue (money : Money) = money.Value
      let getMoneyCurr (money : Money) = money.Currency

      let calcFunc = 
        match (itemXaxis, itemYaxis) with
          | (SpotPrice, Value) -> valuationModel.CalculateOtherSpot >> fst >> getMoneyValue
          | (StrikePrice, Value) -> valuationModel.CalculateOtherStrike >> fst >> getMoneyValue
          | (Volatility, Value) -> valuationModel.CalculateOtherVolatility >> fst >> getMoneyValue
          | (Drift, Value) -> valuationModel.CalculateOtherDrift >> fst >> getMoneyValue
          | (SpotPrice, Delta) -> valuationModel.CalculateOtherSpot >> snd
          | (StrikePrice, Delta) -> valuationModel.CalculateOtherStrike >> snd
          | (Volatility, Delta) -> valuationModel.CalculateOtherVolatility >> snd
          | (Drift, Delta) -> valuationModel.CalculateOtherDrift >> snd

      //step 1: have a sequence of values (x,y)
      let series =
        let step = (scopeXhigh - scopeXlow) / 200.
        seq {
          for i in scopeXlow .. step .. scopeXhigh do
            yield float i, calcFunc (float i)
        }

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

    //step 4: add or replace series on existing chart
    { ChartData.Default with 
        Series = Array.map makeSeries eoTrade
        Title = "Chart"
        ItemX = itemXaxis
        ItemY = itemYaxis
        Trades = eoTradeID
        ScopeX = scopeX
        Data = data
        MarketData = marketData
    }


let mkDummyChart () : ChartData = 

    let r = Random()
    let mkDummySeries () : Series = 
      let predefinedChartFunctions = [| (fun x -> 20.0*(sin x)); (fun x -> x); (fun x -> x*x) |] 
      let multiplier = r.NextDouble()
      let mapFun = predefinedChartFunctions.[ r.Next(predefinedChartFunctions.Length) ]
      let name = sprintf "Test series %0.2f" multiplier

      //step 1: have a sequence of values (x,y)
      let series =
           seq {
             for i in 1 .. 10 do
               yield float i,mapFun(multiplier * float i) 
           }

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

    //step 4: add or replace series on existing chart
    { ChartData.Default with 
        Series = [|mkDummySeries (); mkDummySeries () |]
        Title = "Dummy Demo Chart"
    }