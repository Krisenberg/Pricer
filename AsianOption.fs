module AsianOption
open System
open Configuration
open Money
open MathNet.Numerics
open MathNet.Numerics.Distributions

type OptionType =
    | Call
    | Put

type ValuationMethod =
    | MonteCarlo

type StrikeType =
    | Fixed
    | Floating

let parseOptionType (optionType: string) : OptionType option =
    match optionType with
    | "Call" -> Some Call
    | "Put" -> Some Put
    | _ -> None

let parseValuationMethod (valuationMethod: string) : ValuationMethod option =
    match valuationMethod with
    | "Monte Carlo" -> Some MonteCarlo
    | _ -> None

let parseStrikeType (strikeType: string) : StrikeType option =
    match strikeType with
    | "Fixed" -> Some Fixed
    | "Floating" -> Some Floating
    | _ -> None

let valuationMethodToString (valuationMethod : ValuationMethod) =
    match valuationMethod with
    | MonteCarlo -> "Monte Carlo"

let strikeTypeToString (strikeType : StrikeType) =
    match strikeType with
    | Fixed -> "Fixed"
    | Floating -> "Floating"


type AsianOptionRecord =
    {
        TradeName       : string
        Asset           : string
        Strike          : float
        Expiry          : DateTime
        Currency        : string
        StrikeType      : StrikeType
        ValuationMethod : ValuationMethod
        OptionType      : OptionType
        Value           : Money option
    }

    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) = 
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if marketData.ContainsKey "valuation::knownCurrencies" 
                              then marketData.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        {
            TradeName       = sprintf "AsianOption%04d" (AsianOptionRecord.sysRandom.Next(9999))
            Asset           = "AAPL"
            Strike          = AsianOptionRecord.sysRandom.Next(10,500)
            Expiry          = (DateTime.Now.AddMonths (AsianOptionRecord.sysRandom.Next(1, 6))).Date
            Currency        = knownCurrencies.[ AsianOptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            StrikeType      = Fixed
            ValuationMethod = MonteCarlo
            OptionType      = Call
            Value           = None
        }

type AsianOptionValuationInputs = 
    {
        Trade : AsianOptionRecord
        Data : Configuration
        MarketData: MarketData
        AssetsData: AssetsData
    }


type AsianOptionValuationModel(inputs: AsianOptionValuationInputs) = 
    let (assetSpot, assetVol) = assetValues inputs.Trade.Asset inputs.AssetsData
    member this.valuationMethod =
        match inputs.Trade.ValuationMethod with
        | MonteCarlo -> this.calculateMonteCarlo

    member this.drift = this.convertPercentage(marketDrift inputs.MarketData)
    member this.volatility = this.convertPercentage(assetVol)
    member this.time = this.calculateMaturity(inputs.Trade.Expiry)
    member this.currencies = this.prepareCurrencies()
    member this.fxRate = this.currencies |> fst
    member this.finalCcy = this.currencies |> snd

    member this.calculateMaturity(expiry : DateTime) : float =
        let today = DateTime.Now
        let difference = expiry - today
        let totalDaysInYear = 365.0
        let partOfYear = difference.TotalDays / totalDaysInYear
        partOfYear
        
    member this.prepareCurrencies() : float * ConfigValue = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.Data.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.MarketData.ContainsKey fxRateKey then float inputs.MarketData.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.MarketData.ContainsKey fxRateKey then targetCcy else tradeCcy

        (fxRate, finalCcy)

    member this.convertPercentage(number : float) : float = number / 100.0

    member this.generateSNV (stepsNumber: int)(random: Random) =
        let listOfNumbers = Array.create stepsNumber 0.
        let mutable i = 0
        while (i<stepsNumber) do
            let U1 = random.NextDouble()
            let U2 = random.NextDouble()
            let Z1 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Sin(2. * Math.PI * U2)
            listOfNumbers.[i]<-Z1
            if i<(stepsNumber-1) then
                let Z2 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Cos(2. * Math.PI * U2)
                i <- i+1
                listOfNumbers.[i]<-Z2
            i <- i+1
        listOfNumbers

    member this.simulateAvgPrice (count: int, steps: int, price: float, drift: float, vol: float, years: float) =
        let random = new Random()
        let timeInterval = years / ((float)steps)

        let exp = timeInterval * (drift - ((vol * vol)/2.))
        let power = Math.Pow(Math.E, exp)
        let volDT = vol*Math.Sqrt(timeInterval)

        let mutable avg_prices_sum = 0.0
        let mutable last_prices_sum = 0.0

        for i in 0..(count-1) do
            let mutable actPrice = price
            let standardNormalVariables = this.generateSNV steps random

            let mutable priceSum = 0.0

            for j in 1..steps do
                let newPrice = actPrice * power * Math.Pow(Math.E, (volDT * standardNormalVariables.[(j-1)]))
                priceSum <- priceSum + newPrice
                actPrice <- newPrice
            
            let avg_price = (priceSum / (float steps))
            last_prices_sum <- last_prices_sum + actPrice
            avg_prices_sum <- avg_prices_sum + avg_price
        
        let discountFactor = Math.Exp((-1.0) * drift * years)
        let avgPrice = (avg_prices_sum / (float count)) * discountFactor
        let lastPrice = (last_prices_sum / (float count)) * discountFactor
        
        avgPrice, lastPrice


    member this.parseRunsNumber() =

        let runs = 
            match inputs.Data.TryFind "monteCarlo::AOruns" with
            | Some number -> number
            | None -> "1000"
        
        let runsNumberOpt = runs |> Int32.TryParse |> Utils.ofBool

        let runsNumber = 
            match runsNumberOpt with
            | Some number -> number
            | None -> 1000

        runsNumber

    member this.parseStepsNumber() =

        let steps = 
            match inputs.Data.TryFind "monteCarlo::AOsteps" with
            | Some number -> number
            | None -> "100"
        
        let stepsNumberOpt = steps |> Int32.TryParse |> Utils.ofBool

        let stepsNumber = 
            match stepsNumberOpt with
            | Some number -> number
            | None -> 100

        stepsNumber

    member this.parseFactor() =

        let factor = 
            match inputs.Data.TryFind "asianOption::factor" with
            | Some number -> number
            | None -> "1.0"
        
        let factorNumberOpt = factor |> Double.TryParse |> Utils.ofBool

        let factorNumber = 
            match factorNumberOpt with
            | Some number -> number
            | None -> 1.0

        factorNumber

    member this.calculateMonteCarlo(spot, strike, drift, volatility, time) =

        let simRuns = this.parseRunsNumber()
        let simSteps = this.parseStepsNumber()
        let factor = this.parseFactor()

        let avgPrice, lastPrice = this.simulateAvgPrice(simRuns, simSteps, spot, drift, volatility, time)
        
        let payoff =
            match inputs.Trade.OptionType with
            | Call ->
                match inputs.Trade.StrikeType with
                | Fixed -> Math.Max(avgPrice - strike, 0.0)
                | Floating -> Math.Max(lastPrice - (factor * avgPrice), 0.0)
            | Put ->
                match inputs.Trade.StrikeType with
                | Fixed -> Math.Max(strike - avgPrice, 0.0)
                | Floating -> Math.Max((factor * avgPrice) - lastPrice, 0.0)

        payoff


    member this.Calculate(strike): Money =
        let value= this.valuationMethod(assetSpot, strike, this.drift, this.volatility, this.time)

        { Value = (value / this.fxRate); Currency = this.finalCcy }