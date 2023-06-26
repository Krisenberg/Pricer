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
        SpotPrice       : float
        Strike          : float
        Drift           : float
        Volatility      : float
        Expiry          : DateTime
        Currency        : string
        ValuationMethod : ValuationMethod
        OptionType      : OptionType
        Value           : Money option
        Delta           : float option
    }
    
    (* Simple utility method for creating a random payment. *)
    static member sysRandom = System.Random()
    static member Random(marketData : MarketData) = 
        (* We pick a random currency either from given short list, or from valuation::knownCurrencies config key *)
        let knownCurrenciesDefault = [| "EUR"; "USD"; "PLN"; |]
        
        let knownCurrencies = if marketData.ContainsKey "valuation::knownCurrencies" 
                              then marketData.["valuation::knownCurrencies"].Split([|' '|])
                              else knownCurrenciesDefault
        
        
        {
            TradeName       = sprintf "EuropeanOption%04d" (EuropeanOptionRecord.sysRandom.Next(9999))
            SpotPrice       = EuropeanOptionRecord.sysRandom.Next(10,500)
            Strike          = EuropeanOptionRecord.sysRandom.Next(10,500)
            Drift           = EuropeanOptionRecord.sysRandom.Next(0,30)
            Volatility      = EuropeanOptionRecord.sysRandom.Next(0,30)
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
    }

(* The valuation model for Payment. We may have multiple valuation models implementations per given trade type, or have a valuation model that handles multiple trade types. *)
type EuropeanOptionValuationModel(inputs: EuropeanOptionValuationInputs) = 
    (* Calculate() method returns a value of given trade. This one is very simple, yet demonstrates some concepts.
    
    It will try to return the result in the global default currency as configured by valuation::baseCurrency key.

    If the valuation::baseCurrency is not defined or we are unable to obtain the FX rate FX::<targetCcy><tradeCcy>, 
    we simply return the value using the trade currency.

    *)
    member this.valuationMethods = 
        dict<ValuationMethod * OptionType, float*float*float*float -> float*float>[(Formulas, Call), this.calculateFormulas;(Formulas, Put), this.calculateFormulas; (MonteCarlo, Call), this.calculateMonteCarlo;(MonteCarlo, Put), this.calculateMonteCarlo]
    member this.drift = inputs.Trade.Drift / 100.0
    member this.volatility = inputs.Trade.Volatility / 100.0
    member this.time = this.calculateMaturity()

    member this.fxRate = this.PrepareCurrencies() |> fst
    member this.finalCcy = this.PrepareCurrencies() |> snd

    member this.calculateMaturity() : float =
        let today = DateTime.Now
        let difference = inputs.Trade.Expiry - today
        let totalDaysInYear = 365.0 // Assuming a non-leap year
        let partOfYear = difference.TotalDays / totalDaysInYear
        partOfYear
        
    member this.PrepareCurrencies() : float * ConfigValue = 
        let tradeCcy = inputs.Trade.Currency
        // let tradeCcy = inputs.Trade.BaseCurrency

        // let targetCcy = match inputs.MarketData.TryFind "valuation::baseCurrency" with
        //                  | Some ccy -> ccy
        //                  | None -> tradeCcy
        let targetCcy = match inputs.Data.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        // let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        // let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy
        let fxRate = if inputs.MarketData.ContainsKey fxRateKey then float inputs.MarketData.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.MarketData.ContainsKey fxRateKey then targetCcy else tradeCcy

        (fxRate, finalCcy)

    // member this.calculateD1Formula() : float =
    //     let numerator = (log(inputs.Trade.SpotPrice/inputs.Trade.Strike) + (this.drift + (this.volatility ** 2.0)/2.0) * this.time)
    //     let denominator = (this.volatility * sqrt(this.time))
    //     (numerator/denominator)

    member this.calculateD1Formula(spot, strike, drift, volatility) : float =
        let numerator = (log(spot/strike) + (drift + (volatility ** 2.0)/2.0) * this.time)
        let denominator = (volatility * sqrt(this.time))
        (numerator/denominator)


    // member this.calculateFormulas() : float*float =
    //     let d1 = this.calculateD1Formula()
    //     let d1CDF = Normal.CDF(0.0, 1.0, d1)
    //     let d2CDF = Normal.CDF(0.0, 1.0, d1 - (this.volatility * sqrt(this.time)))
    //     let neg_d1CDF = Normal.CDF(0.0, 1.0, (-1.0) * d1)
    //     let neg_d2CDF = Normal.CDF(0.0, 1.0, (this.volatility * sqrt(this.time)) - d1)
    //     let power = exp((-1.0) * this.drift * this.time)

    //     let value =
    //         match inputs.Trade.OptionType with
    //         | Call -> (inputs.Trade.SpotPrice *d1CDF) - (inputs.Trade.Strike * power) * d2CDF
    //         | Put -> (inputs.Trade.Strike * power * neg_d2CDF) - (inputs.Trade.SpotPrice * neg_d1CDF)

    //     let delta =
    //         match inputs.Trade.OptionType with
    //         | Call -> d1CDF
    //         | Put -> (-1.0) * neg_d1CDF

    //     (value, delta)


    member this.calculateFormulas(spot, strike, drift, volatility) : float*float =
        let d1 = this.calculateD1Formula(spot, strike, drift, volatility)
        let d1CDF = Normal.CDF(0.0, 1.0, d1)
        let d2CDF = Normal.CDF(0.0, 1.0, d1 - (volatility * sqrt(this.time)))
        let neg_d1CDF = Normal.CDF(0.0, 1.0, (-1.0) * d1)
        let neg_d2CDF = Normal.CDF(0.0, 1.0, (volatility * sqrt(this.time)) - d1)
        let power = exp((-1.0) * drift * this.time)

        let value =
            match inputs.Trade.OptionType with
            | Call -> (spot *d1CDF) - (strike * power) * d2CDF
            | Put -> (strike * power * neg_d2CDF) - (spot * neg_d1CDF)

        let delta =
            match inputs.Trade.OptionType with
            | Call -> d1CDF
            | Put -> (-1.0) * neg_d1CDF

        (value, delta)

    member this.generateSNV (steps: int)(random: Random) =
        let listOfNumbers = Array.create steps 0.
        let mutable i = 0
        while (i<steps) do
            let U1 = random.NextDouble()
            let U2 = random.NextDouble()
            let Z1 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Sin(2. * Math.PI * U2)
            listOfNumbers.[i]<-Z1
            if i<(steps-1) then
                let Z2 = Math.Sqrt((-2.) * Math.Log(U1, Math.E)) * Math.Cos(2. * Math.PI * U2)
                i <- i+1
                listOfNumbers.[i]<-Z2
            i <- i+1
        listOfNumbers

    member this.simulate (count: int, steps: int, spot: float, drift: float, vol: float, years: float) =
        let random = new Random()
        let timeInterval = years / ((float)steps)
        let payoffs = Array.create count 0.

        for i in 0..(count-1) do
            let mutable actPrice = spot
            let standardNormalVariables = this.generateSNV steps random

            for j in 1..steps do
                let power = (drift - ((vol * vol)/2.))*timeInterval + vol*Math.Sqrt(timeInterval)*standardNormalVariables.[(j-1)]
                let newPrice = actPrice * Math.Pow(Math.E, power)
                actPrice <- newPrice

            let diff =
                match inputs.Trade.OptionType with
                | Call -> actPrice - inputs.Trade.Strike
                | Put -> inputs.Trade.Strike - actPrice
            payoffs.[i]<-if (diff > 0.) then diff else 0.

        Array.average payoffs

    member this.calculateMonteCarlo(spot, strike, drift, volatility) =
        let runs = 
            match inputs.Data.TryFind "monteCarlo::runs" with
            | Some number -> number
            | None -> "100"
        
        let runsNumberOpt = runs |> Int32.TryParse |> Utils.ofBool
        let runsNumber = 
            match runsNumberOpt with
            | Some number -> number
            | None -> 10

        let steps = 
            match inputs.Data.TryFind "monteCarlo::simulationSteps" with
            | Some number -> number
            | None -> "10"
        
        let stepsNumberOpt = steps |> Int32.TryParse |> Utils.ofBool
        let stepsNumber = 
            match stepsNumberOpt with
            | Some number -> number
            | None -> 10

        let value = this.simulate(runsNumber, stepsNumber, spot, drift, volatility, this.time)

        let d1 = this.calculateD1Formula(spot, strike, drift, volatility)
        let d1CDF = Normal.CDF(0.0, 1.0, d1)
        let neg_d1CDF = Normal.CDF(0.0, 1.0, (-1.0) * d1)
        let delta =
            match inputs.Trade.OptionType with
            | Call -> d1CDF
            | Put -> (-1.0) * neg_d1CDF

        (value, delta)


    member this.Calculate() : Money*float = 
        //let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, this.drift, this.volatility)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta


    member this.CalculateOtherSpot(spotPrice : float) : Money * float =
        //let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(spotPrice, inputs.Trade.Strike, this.drift, this.volatility)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta

    member this.CalculateOtherStrike(strike : float) : Money * float =
        //let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, strike, this.drift, this.volatility)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta

    member this.CalculateOtherDrift(drift : float) : Money * float =
        //let fxRate, finalCcy = this.PrepareCurrencies()
        let scaledDrift = drift / 100.0
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, scaledDrift, this.volatility)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta

    member this.CalculateOtherVolatility(volatility : float) : Money * float =
        //let fxRate, finalCcy = this.PrepareCurrencies()
        let scaledVolatility = volatility / 100.0
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, this.drift, scaledVolatility)

        { Value = (value / this.fxRate); Currency = this.finalCcy }, delta
    
    // member this.CalculateMonteCarlo() : Money =