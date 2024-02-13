module Valuation
open Trades
open Payment
open EuropeanOption
open AsianOption

let valuateTrade config marketData assetsData (trade : Trade) : Trade =
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
          AssetsData = assetsData
        }
      let vm = EuropeanOptionValuationModel(inputs)
      // let drift = Configuration.marketDrift marketData
      // let (assetSpot, assetVol) = Configuration.assetValues inputs.Trade.Asset assetsData
      let value, delta = vm.Calculate(eo.Strike)
      // let value, delta = vm.Calculate(assetSpot, eo.Strike, drift, assetVol, eo.Expiry)
      EuropeanOption { eo with 
                          Value = Some <| value
                          Delta = Some <| delta}
  | AsianOption ao ->
      let inputs : AsianOptionValuationInputs =
        { Trade = ao
          Data = config
          MarketData = marketData
        }
      let vm = AsianOptionValuationModel(inputs)
      let value = vm.Calculate(ao.SpotPrice, ao.Strike, ao.Drift, ao.Volatility, ao.Expiry)
      AsianOption { ao with Value = Some <| value }



