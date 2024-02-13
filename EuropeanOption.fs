module EuropeanOption
open System
open Configuration
open Money
open MathNet.Numerics
open MathNet.Numerics.Distributions

type OptionType =
    | Call
    | Put

type ValuationMethod =
    | Formulas
    | MonteCarlo

let parseOptionType (optionType: string) : OptionType option =
    match optionType with
    | "Call" -> Some Call
    | "Put" -> Some Put
    | _ -> None

let parseValuationMethod (valuationMethod: string) : ValuationMethod option =
    match valuationMethod with
    | "Analytical" -> Some Formulas
    | _ -> Some MonteCarlo

let valuationMethodToString (valuationMethod : ValuationMethod) =
    match valuationMethod with
    | Formulas -> "Analytical"
    | MonteCarlo -> "Monte Carlo"

// Model for European Option trades.
type EuropeanOptionRecord =
    {
        TradeName       : string
        Asset           : string
        Strike          : float
        Expiry          : DateTime
        Currency        : string
        ValuationMethod : ValuationMethod
        OptionType      : OptionType
        Value           : Money option
        Delta           : float option
    }
    
    (* Creating random EO *)
    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if marketData.ContainsKey "valuation::knownCurrencies" 
                              then marketData.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        {
            TradeName       = sprintf "EuropeanOption%04d" (EuropeanOptionRecord.sysRandom.Next(9999))
            Asset           = "AAPL"
            Strike          = EuropeanOptionRecord.sysRandom.Next(10,500)
            Expiry          = (DateTime.Now.AddMonths (EuropeanOptionRecord.sysRandom.Next(1, 6))).Date
            Currency        = knownCurrencies.[ EuropeanOptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            ValuationMethod = Formulas
            OptionType      = Call
            Value           = None
            Delta           = None
        }

(* Complete set of data required for valuation *)
type EuropeanOptionValuationInputs = 
    {
        Trade : EuropeanOptionRecord
        Data : Configuration
        MarketData: MarketData
        AssetsData: AssetsData
    }


type EuropeanOptionValuationModel(inputs: EuropeanOptionValuationInputs) = 
    let (assetSpot, assetVol) = assetValues inputs.Trade.Asset inputs.AssetsData
    member this.valuationMethod =
        match inputs.Trade.ValuationMethod with
        | Formulas -> this.calculateFormulas
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

        let fxRate = if inputs.MarketData.ContainsKey fxRateKey then float inputs.MarketData.[ fxRateKey ] else 1.0
        let finalCcy = if inputs.MarketData.ContainsKey fxRateKey then targetCcy else tradeCcy

        (fxRate, finalCcy)

    member this.convertPercentage(number : float) : float = number / 100.0

    member this.calculateD1Formula(spot, strike, drift, volatility, time) : float =
        let numerator = (log(spot/strike) + (drift + (volatility ** 2.0)/2.0) * time)
        let denominator = (volatility * sqrt(time))
        (numerator/denominator)

    member this.calculateDelta(spot, strike, drift, volatility, time) : float =
        let d1 = this.calculateD1Formula(spot, strike, drift, volatility, time)
        let d1CDF = Normal.CDF(0.0, 1.0, d1)
        let neg_d1CDF = Normal.CDF(0.0, 1.0, (-1.0) * d1)
        let delta =
            match inputs.Trade.OptionType with
            | Call -> d1CDF
            | Put -> (-1.0) * neg_d1CDF
        
        delta

    member this.calculateFormulas(spot, strike, drift, volatility, time) : float =
        let d1 = this.calculateD1Formula(spot, strike, drift, volatility, time)
        let d1CDF = Normal.CDF(0.0, 1.0, d1)
        let d2CDF = Normal.CDF(0.0, 1.0, d1 - (volatility * sqrt(time)))
        let neg_d1CDF = Normal.CDF(0.0, 1.0, (-1.0) * d1)
        let neg_d2CDF = Normal.CDF(0.0, 1.0, (volatility * sqrt(time)) - d1)
        let power = exp((-1.0) * drift * time)

        let value =
            match inputs.Trade.OptionType with
            | Call -> (spot *d1CDF) - (strike * power) * d2CDF
            | Put -> (strike * power * neg_d2CDF) - (spot * neg_d1CDF)
        
        value

    member this.generateSNV (runsNumber: int)(random: Random) =
        let listOfNumbers = Array.create runsNumber 0.
        let mutable i = 0
        while (i<runsNumber) do
            let U1 = random.NextDouble()
            let U2 = random.NextDouble()
            let Z1 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Sin(2. * Math.PI * U2)
            listOfNumbers.[i]<-Z1
            if i<(runsNumber-1) then
                let Z2 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Cos(2. * Math.PI * U2)
                i <- i+1
                listOfNumbers.[i]<-Z2
            i <- i+1
        listOfNumbers

    member this.parseRunsNumber() =

        let runs = 
            match inputs.Data.TryFind "monteCarlo::EOruns" with
            | Some number -> number
            | None -> "1000"
        
        let runsNumberOpt = runs |> Int32.TryParse |> Utils.ofBool

        let runsNumber = 
            match runsNumberOpt with
            | Some number -> number
            | None -> 1000

        runsNumber

    member this.calculateMonteCarlo(spot, strike, drift, volatility, time) =
        
        let runsNumber = this.parseRunsNumber()

        let base_power = time * (drift - ((volatility * volatility)/2.0))
        let S_base = spot * Math.Pow(Math.E, base_power)
        let mutable payoff_sum = 0.0

        let random = new Random()

        let standardNormalVariables = this.generateSNV runsNumber random

        for i in 0..(runsNumber-1) do
            let power = volatility * Math.Sqrt(time) * standardNormalVariables.[i]
            let generated_S = S_base * Math.Pow(Math.E, power)

            let payoff =
                match inputs.Trade.OptionType with
                | Call -> Math.Max(generated_S - inputs.Trade.Strike, 0.0)
                | Put -> Math.Max(inputs.Trade.Strike - generated_S, 0.0)

            payoff_sum <- (payoff_sum + payoff)

        let discount_factor = Math.Pow(Math.E, (-1.0) * drift * time)

        let value = (payoff_sum / (float <| runsNumber)) * discount_factor

        value


    member this.Calculate(asset, strike, time) : Money*float =
        let value= this.valuationMethod(assetSpot, strike, this.drift, this.volatility, this.time)
        let delta = this.calculateDelta(assetSpot, strike, this.drift, this.volatility, this.time)
        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta

    member this.Simulate(spot, strike, drift, volatility, time) : Money*float =
        let scaled_drift = this.convertPercentage(drift)
        let scaled_volatility = this.convertPercentage(volatility)
        let calculated_time = this.calculateMaturity(time)

        let value= this.valuationMethod(spot, strike, scaled_drift, scaled_volatility, calculated_time)
        let delta = this.calculateDelta(spot, strike, scaled_drift, scaled_volatility, calculated_time)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta