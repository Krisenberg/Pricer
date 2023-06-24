module Update

open Configuration
open Elmish
open Messages
open Model
open Payment
open EuropeanOption
open Chart
open System
open System.Net.Http
open System.Net.Http.Json
open Trades
open Valuation

let changeTrade (trades : Map<TradeID,UITrade>) id f =
        match Map.tryFind id trades with
        | Some t -> 
            match f t with
            | Some t' -> Map.add id t' trades, Cmd.none
            | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not update trade %s (%A)" t.Name id)
        | None -> trades, Cmd.ofMsg <| Warning (sprintf "could not find trade %A" id)

let tradeChangeUpdate (model : Model) = function
    | NewName (id,name) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with TradeName = name}
                                | EuropeanOption eo -> Some <| EuropeanOption { eo with TradeName = name}
                            )
            )

    | NewPrincipal (id,principal) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    Int64.TryParse(principal)
                                    |> Utils.ofBool
                                    |> Option.map (fun principal ->
                                            Payment { p with Principal = principal})
                                | _ -> None
                            )
            )

    | NewExpiry (id,expiry) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            Payment { p with Expiry = expiry})
                                | EuropeanOption eo -> 
                                    DateTime.TryParse(expiry)
                                    |> Utils.ofBool
                                    |> Option.map (fun expiry ->
                                            EuropeanOption { eo with Expiry = expiry})
                            )
            )

    | NewCurrency (id,ccy) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | Payment p -> Some <| Payment { p with Currency = ccy}
                                | EuropeanOption eo -> Some <| EuropeanOption { eo with Currency = ccy}))

    | NewSpotPrice (id,spot) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(spot)
                                    |> Utils.ofBool
                                    |> Option.map (fun spot ->
                                            EuropeanOption { eo with SpotPrice = spot})
                                | _ -> None))

    | NewStrike (id,strike) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(strike)
                                    |> Utils.ofBool
                                    |> Option.map (fun strike ->
                                            EuropeanOption { eo with Strike = strike})
                                | _ -> None))

    | NewDrift (id,drift) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(drift)
                                    |> Utils.ofBool
                                    |> Option.map (fun drift ->
                                            EuropeanOption { eo with Drift = drift})
                                | _ -> None))

    | NewVolatility (id,volatility) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(volatility)
                                    |> Utils.ofBool
                                    |> Option.map (fun volatility ->
                                            EuropeanOption { eo with Volatility = volatility})
                                | _ -> None))

    | NewValuationMethod (id,valuationMethod) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    parseValuationMethod(valuationMethod)
                                    |> Option.map (fun valMet -> 
                                            EuropeanOption { eo with ValuationMethod = valMet})
                                | _ -> None))

    | NewOptionType (id,optionType) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo ->
                                    parseOptionType(optionType)
                                    |> Option.map (fun optType -> 
                                            EuropeanOption { eo with OptionType = optType})
                                | _ -> None))

let getTradesArray (trades : Map<_,UITrade>) =
    onlyEuropeanOptions(trades) |> List.map (fun (_, eoRecord) -> eoRecord) |> Array.ofList
    //model.trades |> Map.toSeq |> Seq.map (fun (_, uiTrade) -> uiTrade.trade) |> Array.ofSeq

let chartChangeUpdate (model : Model) = function
    | NewItemXaxis item -> parseItemXaxis(item)
                                   |> Option.map (fun optItem ->
                                        { model with chart=makeEuropeanOptionsChart(optItem, model.chart.ItemY, getTradesArray(model.trades), model.chart.ScopeX, model.configuration, model.marketData)})
    | NewItemYaxis item -> parseItemYaxis(item)
                                   |> Option.map (fun optItem ->
                                        { model with chart=makeEuropeanOptionsChart(model.chart.ItemX, optItem, getTradesArray(model.trades), model.chart.ScopeX, model.configuration, model.marketData)})
    | NewScopeX (newScope) -> 
                                    let (scopeXlow, scopeXhigh)=(newScope.Split(';').[0], newScope.Split(';').[1])
                                    (scopeXlow, scopeXhigh)
                                    |> Utils.tryParseTupleFloats
                                    |> Utils.ofBoolTuple
                                    |> Utils.optionMapTuple (fun (xLow, xHigh) ->
                                        { model with chart=makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, getTradesArray(model.trades), (xLow, xHigh), model.configuration, model.marketData)})

let update (http: HttpClient) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | AddPayment ->
        let newPayment = Trades.wrap (Payment <| PaymentRecord.Random(model.configuration))
        let newTrades = Map.add newPayment.id newPayment model.trades
        { model with trades = newTrades }, Cmd.none
    | AddEuropeanOption ->
        let newEuropeanOption = Trades.wrap (EuropeanOption <| EuropeanOptionRecord.Random(model.configuration))
        let newTrades = Map.add newEuropeanOption.id newEuropeanOption model.trades
        { model with 
            trades = newTrades
            chart = makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, getTradesArray(newTrades), model.chart.ScopeX, model.configuration, model.marketData)
        }, Cmd.none
    | RemoveTrade(tradeId) ->
        let newTrades = Map.remove tradeId model.trades
        { model with 
            trades = newTrades
            chart = makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, getTradesArray(newTrades), model.chart.ScopeX, model.configuration, model.marketData)
        }, Cmd.none
    | TradeChange msg ->
        let newTrades,cmd = tradeChangeUpdate model msg
        { model with 
            trades = newTrades
            chart = makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, getTradesArray(newTrades), model.chart.ScopeX, model.configuration, model.marketData)
        }, Cmd.batch [cmd; Cmd.ofMsg RecalculateAll]
    | ConfigChange (key,value) ->
        let config = model.configuration
        let config' = Map.add key value config
        { model with configuration = config'}, Cmd.none
    | MarketDataChange (key,value) ->
        let md = model.marketData
        let md' = Map.add key value md
        { model with marketData = md'}, Cmd.ofMsg RecalculateAll
    | GotMarketData response ->
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with marketData = c }, Cmd.none
    | LoadData ->
        let getConfig() = http.GetFromJsonAsync<JsonConfig>("/configuration.json")
        let conf = Cmd.OfTask.either getConfig () GotConfig Error
        let getMDConfig() = http.GetFromJsonAsync<JsonConfig>("/marketDataConfig.json")
        let mdConf = Cmd.OfTask.either getMDConfig () GotMarketData Error
        { model with configuration = Map.empty }, Cmd.batch [conf; mdConf]
    | GotConfig response -> 
        let c = response |> 
                Array.collect (fun cat -> 
                        cat.Config 
                        |> Array.map (fun {Key = k; Value = v} ->
                            sprintf "%s::%s" cat.Category k, v))
                |> Map.ofArray
        { model with configuration = c }, Cmd.none
    | RecalculateAll ->
        let trades =
             model.trades
             |> Map.map (fun _ -> Trades.map <| valuateTrade model.marketData model.configuration)
        { model with 
            trades = trades
            chart = makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, getTradesArray(trades), model.chart.ScopeX, model.configuration, model.marketData)
        }, Cmd.none
    | EOChartChange msg -> 
        let updatedModelOption = chartChangeUpdate model msg
        let result =
            match updatedModelOption with
            | Some newModel -> newModel, Cmd.none
            | None -> model, Cmd.none
        result
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | Warning err ->
        { model with error = Some err }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
