module Valuation
open Trades
open Payment
open EuropeanOption

let valuateTrade config marketData (trade : Trade) : Trade =
  match trade with
  | Payment p -> 
      let inputs : PaymentValuationInputs = 
        { Trade = p
          Data = config
          MarketData = marketData
        }
      let vm = PaymentValuationModel(inputs)
      Payment { p with Value = Some <| vm.Calculate()}

  | EuropeanOption eo ->
      let inputs : EuropeanOptionValuationInputs =
        { Trade = eo
          Data = config
          MarketData = marketData
        }
      let vm = EuropeanOptionValuationModel(inputs)
      let value, delta = vm.Calculate(eo.SpotPrice, eo.Strike, eo.Drift, eo.Volatility, eo.Expiry)
      EuropeanOption { eo with 
                          Value = Some <| value
                          Delta = Some <| delta}



