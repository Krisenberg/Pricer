module View

open Bolero
open Bolero.Html
open Chart
open Messages
open Model
open Money
open Payment
open EuropeanOption
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

let configDisplay = keyValueMapDisplay ConfigChange "Configuration"
        
let marketDataDisplay = keyValueMapDisplay MarketDataChange "Market Data"

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
        }
    let Yaxis =
        let title = comp<RadzenAxisTitle> { "Text" => data.Title}
        comp<RadzenValueAxis>{ attr.fragment "ChildContent" title }
    let childContent = 
          concat {
              for series in data.Series do
                mkSeriesComponent series
              Xaxis
              Yaxis
          }
    comp<RadzenChart> { attr.fragment "ChildContent" childContent }

let summary (model: Model) dispatch =
    let groupedByCCy =
        model.trades
        |> Map.values
        |> Seq.choose (fun x ->
            match x.trade with
            | Payment p -> p.Value
            | EuropeanOption eo -> eo.Value
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
        .Expiry(sprintf "%A" p.Expiry, tradeChange NewExpiry)
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
        .Spot(sprintf "%.2f" eo.SpotPrice,tradeChange NewSpotPrice)
        .Strike(sprintf "%.2f" eo.Strike,tradeChange NewStrike)
        .Drift(sprintf "%.2f" eo.Drift, tradeChange NewDrift)
        .Volatility(sprintf "%.2f" eo.Volatility, tradeChange NewVolatility)
        .Expiry(sprintf "%A" eo.Expiry, tradeChange NewExpiry)
        .Currency(eo.Currency, tradeChange NewCurrency)
        .ValuationMethod(valuationMethodToString eo.ValuationMethod, tradeChange NewValuationMethod)
        .OptionType(string eo.OptionType, tradeChange NewOptionType)
        .Value(value)
        .Delta(delta)
        .Delete(fun e -> dispatch (RemoveTrade tradeId))
        .Elt()

let configureChart dispatch (model : Model, europeanOptions : list<TradeID * EuropeanOptionRecord>)=
    let initialMap = Map.empty
    let addToMap map (tradeID, europeanOptionRecord) =
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


let homePage (model: Model) dispatch =

    let payments = onlyPayments model.trades
    let europeanOptions = onlyEuropeanOptions model.trades
    let paymentsView = 
        Templates.Payments()
            .AddPayment(fun _ -> dispatch AddPayment)
            // .RecalculateAll(fun _ -> dispatch RecalculateAll)
            .RecalculateAll(fun _ -> dispatch RecalculateAllPayments)
            .PaymentRows(forEach payments (paymentRow dispatch))
            .Elt()
    let europeanOptionsView = 
        Templates.EuropeanOptions()
            .AddEuropeanOption(fun _ -> dispatch AddEuropeanOption)
            // .RecalculateAll(fun _ -> dispatch RecalculateAll)
            .RecalculateAll(fun _ -> dispatch RecalculateAllEO)
            .EuropeanOptionsRows(forEach europeanOptions (europeanOptionRow dispatch))
            .Elt()
    

    Templates.Home()
     .SummaryPlaceholder(summary model dispatch)
     .PaymentsPlaceholder(paymentsView)
     .EuropeanOptionsPlaceholder(europeanOptionsView)
     .MarketDataPlaceholder(marketDataDisplay model.marketData dispatch)
     .EOChartConfigPlaceholder(configureChart dispatch (model, europeanOptions))
     .DrawChart(fun _ -> dispatch DrawChart)
     .ChartsPlaceholder(
        if model.showChart then
            plotLineChart model.chart
        else
            Html.empty())
     .Elt()

let menuItem (model: Model) (router :Router<_,_,_>) (page: Page) (text: string) =
    let activeFlag = "rz-button rz-secondary"
    Main.MenuItem()
        .Active(if model.page = page then activeFlag else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view router model dispatch =
    Main()
        .Menu(concat {
            menuItem model router Home "Home"
            menuItem model router Config "Config"
        })
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
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