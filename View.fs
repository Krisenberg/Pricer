module View

open Bolero
open Bolero.Html
open Chart
open Messages
open Model
open Money
open Payment
open EuropeanOption
open AsianOption
open Radzen.Blazor
open System.Collections.Generic
open Trades

type Templates = Template<"wwwroot/templates.html">
type Main = Template<"wwwroot/main.html">

let keyValueMapDisplay msg name (model: Map<string,string>) dispatch =
    let configRow (kvp : KeyValuePair<string,string>) =
        Templates.ConfigRow()
            .Key(kvp.Key)
            .Value(kvp.Value,fun v -> dispatch <| msg (kvp.Key,v))
            .Elt()
    Templates.KeyValueMapDisplay()
        .Title(text name)
        .Rows(forEach model configRow)
        .Elt()

let assetsDisplay msg title (model: Map<string, (string * string)>) dispatch =
    let assetRow (nameSpotVol : KeyValuePair<string, (string * string)>) =
        Templates.AssetRow()
            .Key(nameSpotVol.Key)
            .Spot(nameSpotVol.Value |> fst, fun sp -> dispatch <| msg (nameSpotVol.Key, sp, nameSpotVol.Value |> snd))
            .Vol(nameSpotVol.Value |> snd, fun vol -> dispatch <| msg (nameSpotVol.Key, nameSpotVol.Value |> fst, vol))
            .Elt()
    Templates.AssetsDisplay()
        .Title(text title)
        .Rows(forEach model assetRow)
        .Elt()

let configDisplay = keyValueMapDisplay ConfigChange "Configuration"
        
let marketDataDisplay = keyValueMapDisplay MarketDataChange "Market Data"

let assetsDataDisplay = assetsDisplay AssetsDataChange "Assets Data"

let plotLineChart (data : ChartData) =

    let mkSeriesComponent series =
        comp<RadzenLineSeries<ChartItem>>{
          "Smooth" => series.Smooth;
          "CategoryProperty"=>"XValue";
          "Title"=>series.SeriesName;
          "LineType"=> series.Line
          "ValueProperty"=>"YValue"
          "Data" => series.Values;
          attr.fragment "ChildContent"
            <| concat {
              comp<RadzenSeriesDataLabels>{"Visible" => series.ShowLabels}
              comp<RadzenMarkers>{"MarkerType" => series.Marker}
            }
        }

    let Xaxis = 
        comp<RadzenCategoryAxis>{
            "Padding"=>20.0
            "Min" => fst data.ScopeX
            "Max" => snd data.ScopeX
            "Visible" => true
        }
    let Yaxis =
        let title = comp<RadzenAxisTitle> { "Text" => data.Title}
        comp<RadzenValueAxis>{
            "Visible" => true
            attr.fragment "ChildContent" title 
        }

    let legend = comp<RadzenLegend> { "Position" => LegendPosition.Bottom }
    
    let childContent = 
          concat {
              for series in data.Series do
                mkSeriesComponent series
              Xaxis
              Yaxis
              legend
          }
    
    comp<RadzenChart> {
        "Style" => "width: 1300px; height: 450px"
        attr.fragment "ChildContent" childContent
    }


let summary (model: Model) (name:string) dispatch =
    let groupedByCCy =
        model.trades
        |> Map.values
        |> Seq.choose (fun x ->
            match x.trade with
            | Payment p -> p.Value
            | EuropeanOption eo -> eo.Value
            | AsianOption ao -> ao.Value
            )
        |> Seq.groupBy (fun m -> m.Currency)
    let summaryRow (ccy,values : Money seq) =
        let sum = values |> Seq.sumBy (fun v -> v.Value)
        Templates.SummaryRow()
            .CCY(text ccy)
            .Value(text <| sprintf "%.2f" sum)
            .Elt()
    Templates.Summary()
        .Title(name)
        .Rows(forEach groupedByCCy summaryRow)
        .Elt()

let summaryPayments (model: Model) (name:string) dispatch =
    let groupedByCCy =
        model.trades
        |> Map.values
        |> Seq.choose (fun x ->
            match x.trade with
            | Payment p -> p.Value
            | _ -> None
            )
        |> Seq.groupBy (fun m -> m.Currency)
    let summaryRow (ccy,values : Money seq) =
        let sum = values |> Seq.sumBy (fun v -> v.Value)
        Templates.SummaryRow()
            .CCY(text ccy)
            .Value(text <| sprintf "%.2f" sum)
            .Elt()
    Templates.Summary()
        .Rows(forEach groupedByCCy summaryRow)
        .Elt()


let summaryEuropeanOptions (model: Model) (name:string) dispatch =
    let groupedByCCy =
        model.trades
        |> Map.values
        |> Seq.choose (fun x ->
            match x.trade with
            | EuropeanOption eo -> eo.Value
            | _ -> None
            )
        |> Seq.groupBy (fun m -> m.Currency)
    let summaryRow (ccy,values : Money seq) =
        let sum = values |> Seq.sumBy (fun v -> v.Value)
        Templates.SummaryRow()
            .CCY(text ccy)
            .Value(text <| sprintf "%.2f" sum)
            .Elt()
    Templates.Summary()
        .Rows(forEach groupedByCCy summaryRow)
        .Elt()


let paymentRow dispatch (tradeId, p : PaymentRecord) =
    let value = p.Value |> Option.map (string) |> Option.defaultValue "" 
    let tradeChange msg s = dispatch <| TradeChange (msg (tradeId,s))
    Templates.PaymentsRow()
        .Name(p.TradeName,tradeChange NewName)
        // .Expiry(sprintf "%A" p.Expiry, tradeChange NewExpiry)
        // .Expiry(sprintf "%s" (p.Expiry.ToString("yyyy-MM-dd")), tradeChange NewExpiry)
        .Expiry(p.Expiry.ToString("yyyy-MM-dd"), tradeChange NewExpiry)
        .Currency(p.Currency, tradeChange NewCurrency)
        .Principal(sprintf "%i" p.Principal, tradeChange NewPrincipal)
        .Value(value)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()


let europeanOptionRow dispatch (tradeId, eo : EuropeanOptionRecord) =
    let value = eo.Value |> Option.map (string) |> Option.defaultValue "" 
    let delta = match eo.Delta with
                        | Some delta -> System.Math.Round (delta, 2) |> string
                        | None -> ""

    let tradeChange msg s = dispatch <| TradeChange (msg (tradeId,s))

    Templates.EuropeanOptionsRows()
        .Name(eo.TradeName,tradeChange NewName)
        .Asset(eo.Asset,tradeChange NewAsset)
        .Strike(sprintf "%.2f" eo.Strike,tradeChange NewStrike)
        .Expiry(eo.Expiry.ToString("yyyy-MM-dd"), tradeChange NewExpiry)
        .Currency(eo.Currency, tradeChange NewCurrency)
        .ValuationMethod(EuropeanOption.valuationMethodToString eo.ValuationMethod, tradeChange NewValuationMethod)
        .OptionType(string eo.OptionType, tradeChange NewOptionType)
        .Value(value)
        .Delta(delta)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()


let asianOptionRow dispatch (tradeId, ao : AsianOptionRecord) =
    let value = ao.Value |> Option.map (string) |> Option.defaultValue ""
    let strike =
        match ao.StrikeType with
        | Fixed -> sprintf "%.2f" ao.Strike
        | Floating -> sprintf "%s" "---"

    let tradeChange msg s = dispatch <| TradeChange (msg (tradeId,s))

    Templates.AsianOptionsRows()
        .Name(ao.TradeName,tradeChange NewName)
        .Spot(sprintf "%.2f" ao.SpotPrice,tradeChange NewSpotPrice)
        .Strike(strike,tradeChange NewStrike)
        .Drift(sprintf "%.2f" ao.Drift, tradeChange NewDrift)
        .Volatility(sprintf "%.2f" ao.Volatility, tradeChange NewVolatility)
        .Expiry(sprintf "%A" ao.Expiry, tradeChange NewExpiry)
        .Currency(ao.Currency, tradeChange NewCurrency)
        .StrikeType(AsianOption.strikeTypeToString ao.StrikeType, tradeChange NewStrikeType)
        .ValuationMethod(AsianOption.valuationMethodToString ao.ValuationMethod, tradeChange NewValuationMethod)
        .OptionType(string ao.OptionType, tradeChange NewOptionType)
        .Value(value)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()


let configureChart dispatch (model : Model, europeanOptions : list<TradeID * EuropeanOptionRecord>)=
    let initialMap = Map.empty
    let addToMap map (tradeID, europeanOptionRecord : EuropeanOptionRecord) =
        let tradeName = europeanOptionRecord.TradeName
        Map.add tradeID tradeName map
    let eoMap = List.fold addToMap initialMap europeanOptions 

    let getTradeID (name: string) (tradeIdToTradeNameMap: Map<TradeID, string>) =
        let predicate (key: TradeID) (value: string) =
            value = name
        Map.tryFindKey predicate tradeIdToTradeNameMap

    let updateScopeLow(newVal : string, oldScope : (float*float)) =
        match oldScope with
        | (_, scopeXhigh : float) -> newVal + ";" + string scopeXhigh
    
    let updateScopeHigh(newVal : string, oldScope : (float*float)) =
        match oldScope with
        | (scopeXlow : float, _) -> string scopeXlow + ";" + newVal

    let chartChange msg s = 
        match msg with
        | "NewTrade" -> 
                        let correspondingID = getTradeID s eoMap
                        match correspondingID with
                        | Some tradeID -> dispatch <| EOChartChange (NewTrade tradeID)
                        | None -> ()
        | "NewItemXaxis" -> dispatch <| EOChartChange (NewItemXaxis s)
        | "NewItemYaxis" -> dispatch <| EOChartChange (NewItemYaxis s)
        | "NewScopeLow" ->
                        let newScope = updateScopeLow (s, model.chart.ScopeX)
                        dispatch <| EOChartChange (NewScopeX newScope)
        | _ ->
                        let newScope = updateScopeHigh (s, model.chart.ScopeX)
                        dispatch <| EOChartChange (NewScopeX newScope)

    let getTradeName() =
        if (not (Array.isEmpty model.chart.Trades)) then eoMap.Item(model.chart.Trades.[0]) else ""

    let getLowScope (oldScope : (float*float)) : float =
        match oldScope with
        | (scopeXlow : float, _) -> scopeXlow

    let getHighScope (oldScope : (float*float)) : float=
        match oldScope with
        | (_, scopeXhigh : float) -> scopeXhigh

    let getTradeNames () =
        eoMap.Values |> Seq.toList

    let showName (name: string) =
        Templates.NameSelection()
            .Name(name)
            .Elt()

    let configure() =
        let names = getTradeNames()
        Templates.ChartConfiguration()
            .TradeName(string (getTradeName()), chartChange "NewTrade")
            .Names(forEach names showName)
            .Xaxis(itemXaxisToString model.chart.ItemX,chartChange "NewItemXaxis")
            .Yaxis(string model.chart.ItemY,chartChange "NewItemYaxis")
            .XaxisLow(sprintf "%.2f" (getLowScope(model.chart.ScopeX)), chartChange "NewScopeLow")
            .XaxisHigh(sprintf "%.2f" (getHighScope(model.chart.ScopeX)), chartChange "NewScopeHigh")
            .Elt()

    Templates.EuropeanOptionsChart()
        .Config(configure())
        .Elt()


// let homePage (model: Model) dispatch =

//     let payments = onlyPayments model.trades
//     let europeanOptions = onlyEuropeanOptions model.trades
//     let asianOptions = onlyAsianOptions model.trades

//     let paymentsView = 
//         Templates.Payments()
//             .AddPayment(fun _ -> dispatch AddPayment)
//             // .RecalculateAll(fun _ -> dispatch RecalculateAll)
//             .RecalculateAll(fun _ -> dispatch RecalculateAllPayments)
//             .PaymentRows(forEach payments (paymentRow dispatch))
//             .Elt()
//     let europeanOptionsView = 
//         Templates.EuropeanOptions()
//             .AddEuropeanOption(fun _ -> dispatch AddEuropeanOption)
//             // .RecalculateAll(fun _ -> dispatch RecalculateAll)
//             .RecalculateAll(fun _ -> dispatch RecalculateAllEO)
//             .EuropeanOptionsRows(forEach europeanOptions (europeanOptionRow dispatch))
//             .Elt()
//     let asianOptionsView = 
//         Templates.AsianOptions()
//             .AddAsianOption(fun _ -> dispatch AddAsianOption)
//             // .RecalculateAll(fun _ -> dispatch RecalculateAll)
//             .RecalculateAll(fun _ -> dispatch RecalculateAllAO)
//             .AsianOptionsRows(forEach asianOptions (asianOptionRow dispatch))
//             .Elt()
    

//     Templates.Home()
//      .SummaryPlaceholder(summaryPayments model dispatch)
//      .PaymentsPlaceholder(paymentsView)
//      .EuropeanOptionsPlaceholder(europeanOptionsView)
//      .AsianOptionsPlaceholder(asianOptionsView)
//     //  .MarketDataPlaceholder(marketDataDisplay model.marketData dispatch)
//      .EOChartConfigPlaceholder(configureChart dispatch (model, europeanOptions))
//      .DrawChart(fun _ -> dispatch DrawChart)
//      .ChartsPlaceholder(
//         if model.showChart then
//             plotLineChart model.chart
//         else
//             Html.empty())
//      .Elt()


let paymentsPage (model: Model) dispatch =

    let payments = onlyPayments model.trades

    let paymentsView = 
        Templates.Payments()
            .AddPayment(fun _ -> dispatch AddPayment)
            .RecalculateAll(fun _ -> dispatch RecalculateAllPayments)
            .PaymentRows(forEach payments (paymentRow dispatch))
            .Elt()    

    Templates.PaymentsView()
     .SummaryPlaceholder(summaryPayments model "Summary [Payments]" dispatch)
     .PaymentsPlaceholder(paymentsView)
     .Elt()


let europeanOptionsPage (model: Model) dispatch =

    let europeanOptions = onlyEuropeanOptions model.trades

    let europeanOptionsView = 
        Templates.EuropeanOptions()
            .AddEuropeanOption(fun _ -> dispatch AddEuropeanOption)
            .RecalculateAll(fun _ -> dispatch RecalculateAllEO)
            .EuropeanOptionsRows(forEach europeanOptions (europeanOptionRow dispatch))
            .Elt()    

    Templates.EuropeanOptionsView()
     .SummaryPlaceholder(summaryEuropeanOptions model "Summary [European Options]" dispatch)
     .EuropeanOptionsPlaceholder(europeanOptionsView)
     .EOChartConfigPlaceholder(configureChart dispatch (model, europeanOptions))
     .DrawChart(fun _ -> dispatch DrawChart)
     .ChartsPlaceholder(
        if model.showChart then
            plotLineChart model.chart
        else
            Html.empty())
     .Elt()


let asianOptionsPage (model: Model) dispatch =

    let asianOptions = onlyAsianOptions model.trades

    let asianOptionsView = 
        Templates.AsianOptions()
            .AddAsianOption(fun _ -> dispatch AddAsianOption)
            .RecalculateAll(fun _ -> dispatch RecalculateAllAO)
            .AsianOptionsRows(forEach asianOptions (asianOptionRow dispatch))
            .Elt() 

    Templates.AsianOptionsView()
     .SummaryPlaceholder(summary model "Summary [Asian Options]" dispatch)
     .AsianOptionsPlaceholder(asianOptionsView)
     .Elt()

let marketDataPage (model: Model) dispatch =

    let marketDataView = marketDataDisplay model.marketData dispatch
    let assetsData =
        model.assetsData
        |> Map.map (fun assetKey assetValue ->
            (assetValue.Spot, assetValue.Vol))
    let assetsView = assetsDataDisplay assetsData dispatch

    Templates.MarketDataView()
     .MarketDataPlaceholder(marketDataView)
     .AssetsPlaceholder(assetsView)
     .Elt()


let menuItem (model: Model) (router :Router<_,_,_>) (page: Page) (icon: string) (text: string) =
    let activeFlag = "active-menu-item"
    Main.MenuItem()
        .Active(if model.page = page then activeFlag else "not-active-menu-item")
        .Url(router.Link page)
        .Icon(icon)
        .Text(text)
        .Elt()


let view router model dispatch =
    Main()
        .Menu(concat {
            // menuItem model router Home "fa-house" "Home"
            menuItem model router Summary "fa-list-check" "Summary"
            menuItem model router Payments "fa-money-check-dollar" "Payments"
            menuItem model router EuropeanOptions "fa-earth-europe" "European Options"
            menuItem model router AsianOptions "fa-earth-asia" "Asian Options"
            menuItem model router MarketData "fa-magnifying-glass-chart" "Market data"
            menuItem model router Config "fa-gear" "Config"
        })
        .Body(
            cond model.page <| function
            // | Home -> homePage model dispatch
            | EuropeanOptions -> europeanOptionsPage model dispatch
            | Summary -> summary model "Summary" dispatch
            | Payments -> paymentsPage model dispatch
            | AsianOptions -> asianOptionsPage model dispatch
            | MarketData -> marketDataPage model dispatch
            | Config -> configDisplay model.configuration dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Templates.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()