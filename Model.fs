module Model
open System
open Bolero
open Configuration
open Trades
open Chart

//regular trade with UI specific metadata
//we need unique id to identify trades on UI
type UITrade = 
     {
          trade : Trade
          id : TradeID
     }
     member this.Name =
          match this.trade with
          | Payment p -> p.TradeName
          | EuropeanOption eo -> eo.TradeName
          | AsianOption ao -> ao.TradeName

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] EuropeanOptions
    | [<EndPoint "/asianOptions">] AsianOptions
    | [<EndPoint "/payments">] Payments
    | [<EndPoint "/summary">] Summary
    | [<EndPoint "/marketData">] MarketData
    | [<EndPoint "/config">] Config

/// The Elmish application's model.
type Model =
    {
        page: Page
        trades : Map<TradeID,UITrade>
        marketData: MarketData
        assetsData: AssetsData
        configuration : Configuration
        chart : ChartData
        showChart: bool
        error: string option
    }

    static member Initial = 
      {
          page = EuropeanOptions
          trades = Map.empty
          marketData = Map.empty
          assetsData = Map.empty
          configuration = Map.empty
          chart = ChartData.Default
          showChart = false
          error = None
      }

module Trades =
    let wrap t  =
        { trade = t; id = newTradeID() }

    let map f t = { t with trade = f t.trade }

    let tryMap f t =
        match f t.trade with
        | Some t' -> Some { t with trade = t' }
        | None -> None

    let choose picker (trades : Map<_,UITrade>) : 'a list=
        trades 
        |> Map.values 
        |> List.ofSeq
        |> List.choose picker

    let onlyPayments (trades : Map<_,UITrade>) =
        trades |> choose (fun t -> match t.trade with 
                                    | Payment p -> Some <| (t.id,p)
                                    | _ -> None
                        )

    let onlyEuropeanOptions (trades : Map<_,UITrade>) =
      trades |> choose (fun t -> match t.trade with 
                                  | EuropeanOption eo -> Some <| (t.id,eo)
                                  | _ -> None
                        )
    
    let onlyAsianOptions (trades : Map<_,UITrade>) =
      trades |> choose (fun t -> match t.trade with 
                                  | AsianOption ao -> Some <| (t.id,ao)
                                  | _ -> None
                        )