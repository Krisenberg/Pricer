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
                                    Double.TryParse(spot.Replace('.', ','))
                                    |> Utils.ofBool
                                    |> Option.map (fun spot ->
                                            EuropeanOption { eo with SpotPrice = spot})
                                | _ -> None))

    | NewStrike (id,strike) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(strike.Replace('.', ','))
                                    |> Utils.ofBool
                                    |> Option.map (fun strike ->
                                            EuropeanOption { eo with Strike = strike})
                                | _ -> None))

    | NewDrift (id,drift) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(drift.Replace('.', ','))
                                    |> Utils.ofBool
                                    |> Option.map (fun drift ->
                                            EuropeanOption { eo with Drift = drift})
                                | _ -> None))

    | NewVolatility (id,volatility) ->
        changeTrade model.trades id 
                (Trades.tryMap ( function
                                | EuropeanOption eo -> 
                                    Double.TryParse(volatility.Replace('.', ','))
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


let findEOTrade (trades : Map<TradeID,UITrade>) id =
        match Map.tryFind id trades with
        | Some t -> 
            match t.trade with
            | EuropeanOption eo -> Some eo
            | _ -> None
        | _ -> None

let changeChart (model : Model, newTrades : Map<TradeID,UITrade>) (id : TradeID) =
    if (not (Array.isEmpty model.chart.Trades) && id = model.chart.Trades.[0]) then
        let eoRecordOpt = id |> findEOTrade(newTrades)
        match eoRecordOpt with
        | Some eoRecord -> makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, [|eoRecord|], model.chart.Trades,model.chart.ScopeX, model.configuration, model.marketData)
        | None -> model.chart
    else
        model.chart


let tradeOnChartChangeUpdate (model : Model, newTrades) = function
    | NewName (id,name) -> id |> changeChart(model, newTrades)
    | NewPrincipal (id,principal) -> id |> changeChart(model, newTrades)
    | NewExpiry (id,expiry) -> id |> changeChart(model, newTrades)
    | NewCurrency (id,ccy) -> id |> changeChart(model, newTrades)
    | NewSpotPrice (id,spot) -> id |> changeChart(model, newTrades)
    | NewStrike (id,strike) -> id |> changeChart(model, newTrades)
    | NewDrift (id,drift) -> id |> changeChart(model, newTrades)
    | NewVolatility (id,volatility) -> id |> changeChart(model, newTrades)
    | NewValuationMethod (id,valuationMethod) -> id |> changeChart(model, newTrades)
    | NewOptionType (id,optionType) -> id |> changeChart(model, newTrades)


// let getTradesArray (trades : Map<_,UITrade>) =
//     onlyEuropeanOptions(trades) |> List.map (fun (_, eoRecord) -> eoRecord) |> Array.ofList
//     //model.trades |> Map.toSeq |> Seq.map (fun (_, uiTrade) -> uiTrade.trade) |> Array.ofSeq

// let getTradeWithName (trades: Map<_,UITrade>, newTradeName : string) =
//     onlyEuropeanOptions(trades) |> List.map (fun (_, eoRecord) -> eoRecord) |> List.find (fun eoRecord -> eoRecord.TradeName = newTradeName)

let chartChangeUpdate (model : Model) = function
    | NewTrade tradeID->
                                    tradeID
                                    |> findEOTrade(model.trades)
                                    |> Option.map (fun eoRecord ->
                                    makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, [|eoRecord|], [|tradeID|], model.chart.ScopeX, model.configuration, model.marketData))
    | NewItemXaxis item -> 
                                    let eoRecordOpt = findEOTrade model.trades model.chart.Trades[0]
                                    (eoRecordOpt, parseItemXaxis(item))
                                   |> Utils.optionMapTuple (fun (eoRecord, item) ->
                                    makeEuropeanOptionsChart(item, model.chart.ItemY, [|eoRecord|], model.chart.Trades,model.chart.ScopeX, model.configuration, model.marketData))
    | NewItemYaxis item -> 
                                    let eoRecordOpt = findEOTrade model.trades model.chart.Trades[0]
                                    (eoRecordOpt, parseItemYaxis(item))
                                   |> Utils.optionMapTuple (fun (eoRecord, item) ->
                                    makeEuropeanOptionsChart(model.chart.ItemX, item, [|eoRecord|], model.chart.Trades,model.chart.ScopeX, model.configuration, model.marketData))
    | NewScopeX scope   -> 
                                    let eoRecordOpt = findEOTrade model.trades model.chart.Trades[0]
                                    let (scopeXlow, scopeXhigh)=(scope.Split(';').[0], scope.Split(';').[1])
                                    let parsedScopeLow, parsedScopeHigh = (scopeXlow, scopeXhigh) |> Utils.tryParseTupleFloats |> Utils.ofBoolTuple
                                    (eoRecordOpt,parsedScopeLow, parsedScopeHigh)
                                    |> Utils.optionMapTriple (fun (eoRecord, xLow, xHigh) ->
                                    makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, [|eoRecord|],model.chart.Trades, (xLow, xHigh), model.configuration, model.marketData))

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
        { model with trades = newTrades }, Cmd.none
    | RemoveTrade(tradeId) ->
        let newTrades = Map.remove tradeId model.trades
        if (not (Array.isEmpty model.chart.Trades) && model.chart.Trades.[0] = tradeId) then
            { model with 
                trades = newTrades
                chart = makeEuropeanOptionsChart(model.chart.ItemX, model.chart.ItemY, [||], [||], model.chart.ScopeX, model.configuration, model.marketData)
            }, Cmd.none
        else
            { model with 
                trades = newTrades
            }, Cmd.none
    | TradeChange msg ->
        let newTrades,cmd = tradeChangeUpdate model msg
        let newChart = tradeOnChartChangeUpdate (model, newTrades) msg
        { model with 
            trades = newTrades
            chart = newChart
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
        if (not (Array.isEmpty model.chart.Trades)) then
            { model with 
                trades = trades
                chart = model.chart.Trades.[0] |> changeChart(model, trades)
            }, Cmd.none
        else
            { model with 
                trades = trades
            }, Cmd.none 
    | EOChartChange msg -> 
        let updatedChartOption = chartChangeUpdate model msg
        let result =
            match updatedChartOption with
            | Some newChart -> { model with chart = newChart }, Cmd.none
            | None -> model, Cmd.none
        result
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | Warning err ->
        { model with error = Some err }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none
