module Utils

open System

let ofBool = function
  | true,a -> Some a
  | false,_ -> None

let tryParseTupleFloats (word1 : string, word2 : string) = Double.TryParse word1, Double.TryParse word2

let ofBoolTuple = function
  | ((true,a), (true,b)) -> (Some a, Some b)
  | _ -> (None, None)

let optionMapTuple (func) = function
  | (Some a, Some b) -> Some (func (a,b))
  | _ -> None
