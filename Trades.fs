module Trades
open Payment
open EuropeanOption
open AsianOption

type Trade =
            | Payment of PaymentRecord
            | EuropeanOption of EuropeanOptionRecord
            | AsianOption of AsianOptionRecord

type TradeID = System.Guid

let newTradeID () : TradeID = System.Guid.NewGuid()