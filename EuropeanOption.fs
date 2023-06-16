module EuropeanOption
open System
open Configuration
open Money
open MathNet.Numerics
open MathNet.Numerics.Distributions

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
        ValuationMethod : string
        OptionType      : string
        Value           : Money option
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
            TradeName       = sprintf "Payment%04d" (EuropeanOptionRecord.sysRandom.Next(9999))
            SpotPrice       = EuropeanOptionRecord.sysRandom.Next(50,500)
            Strike          = EuropeanOptionRecord.sysRandom.Next(50,500)
            Drift           = EuropeanOptionRecord.sysRandom.Next(0,30)
            Volatility      = EuropeanOptionRecord.sysRandom.Next(0,30)
            Expiry          = (DateTime.Now.AddMonths (EuropeanOptionRecord.sysRandom.Next(1, 6))).Date
            Currency        = knownCurrencies.[ EuropeanOptionRecord.sysRandom.Next(knownCurrencies.Length) ]
            ValuationMethod = "Formulas"
            OptionType      = "Call"
            Value           = None
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
    member this.valuationMethods = dict<string*string, float -> float>[("Formulas", "Call"), this.calculateCallFormula; ("Formulas", "Put"), this.calculatePutFormula]
    member this.PrepareCurrencies() : float * ConfigValue = 
        let tradeCcy = inputs.Trade.Currency

        let targetCcy = match inputs.MarketData.TryFind "valuation::baseCurrency" with
                         | Some ccy -> ccy
                         | None -> tradeCcy

        let fxRateKey = sprintf "FX::%s%s" targetCcy tradeCcy

        let fxRate = if inputs.Data.ContainsKey fxRateKey then float inputs.Data.[ fxRateKey ] else 1.0 // lookup FX rate
        let finalCcy = if inputs.Data.ContainsKey fxRateKey then targetCcy else tradeCcy

        (fxRate, finalCcy)

    member this.calculateTimeToExpiry() : float =
        let today = DateTime.Now
        let difference = inputs.Trade.Expiry - today
        let totalDaysInYear = 365.0 // Assuming a non-leap year
        let partOfYear = difference.TotalDays / totalDaysInYear
        partOfYear

    member this.calculateD1Formula(time : float) : float =
        let numerator = (log(inputs.Trade.SpotPrice/inputs.Trade.Strike) + (inputs.Trade.Drift + (inputs.Trade.Volatility ** 2.0)/2.0) * time)
        let denominator = (inputs.Trade.Volatility * sqrt(time))
        (numerator/denominator)

    member this.calculateCallFormula(time : float) : float =
        let d1 = this.calculateD1Formula(time)
        let callPrice = (inputs.Trade.SpotPrice * Normal.CDF(0.0, 1.0, d1)) - (inputs.Trade.Strike * (exp((-1.0)*inputs.Trade.Drift*time)) * Normal.CDF(0.0, 1.0, d1 - (inputs.Trade.Volatility * sqrt(time))))
        callPrice

    member this.calculatePutFormula(time : float) : float =
        let d1 = this.calculateD1Formula(time)
        let putPrice = (inputs.Trade.Strike * (exp((-1.0)*inputs.Trade.Drift*time)) * Normal.CDF(0.0, 1.0, (inputs.Trade.Volatility * sqrt(time)) - d1)) - (inputs.Trade.SpotPrice * Normal.CDF(0.0, 1.0, (-1.0) * d1))
        putPrice


    member this.Calculate() : Money = 
        let fxRate, finalCcy = this.PrepareCurrencies()
        let time = this.calculateTimeToExpiry()
        let calculatedValue = this.valuationMethods.Item((inputs.Trade.ValuationMethod, inputs.Trade.OptionType)) time

        { Value = calculatedValue / fxRate; Currency = finalCcy }
    
    // member this.CalculateMonteCarlo() : Money =