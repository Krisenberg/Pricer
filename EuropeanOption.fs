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
    | "Monte Carlo" -> Some MonteCarlo
    | _ -> None

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
        dict<ValuationMethod * OptionType, float*float*float*float -> float*float>[(Formulas, Call), this.calculateFormulas;(Formulas, Put), this.calculateFormulas]
    member this.drift = inputs.Trade.Drift / 100.0
    member this.volatility = inputs.Trade.Volatility / 100.0
    member this.time = this.calculateMaturity()

    member this.calculateMaturity() : float =
        let today = DateTime.Now
        let difference = inputs.Trade.Expiry - today
        let totalDaysInYear = 365.0 // Assuming a non-leap year
        let partOfYear = difference.TotalDays / totalDaysInYear
        partOfYear
        
    member this.PrepareCurrencies() : float * ConfigValue = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.MarketData.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy

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



    member this.Calculate() : Money*float = 
        let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, this.drift, this.volatility)

        { Value = value / fxRate; Currency = finalCcy }, delta


    member this.CalculateOtherSpot(spotPrice : float) : Money * float =
        let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(spotPrice, inputs.Trade.Strike, this.drift, this.volatility)

        { Value = value / fxRate; Currency = finalCcy }, delta

    member this.CalculateOtherStrike(strike : float) : Money * float =
        let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, strike, this.drift, this.volatility)

        { Value = value / fxRate; Currency = finalCcy }, delta

    member this.CalculateOtherDrift(drift : float) : Money * float =
        let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, drift, this.volatility)

        { Value = value / fxRate; Currency = finalCcy }, delta

    member this.CalculateOtherVolatility(volatility : float) : Money * float =
        let fxRate, finalCcy = this.PrepareCurrencies()
        let value, delta = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType))(inputs.Trade.SpotPrice, inputs.Trade.Strike, this.drift, volatility)

        { Value = value / fxRate; Currency = finalCcy }, delta
    
    // member this.CalculateMonteCarlo() : Money =