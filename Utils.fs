module Utils

open System

let ofBool = function
  | true,a -> Some a
  | false,_ -> None

let tryParseTupleFloats (word1 : string, word2 : string) = Double.TryParse (word1.Replace('.', ',')), Double.TryParse (word2.Replace('.', ','))

let ofBoolTuple = function
  | ((true,a), (true,b)) -> (Some a, Some b)
  | _ -> (None, None)

let optionMapTuple (func) = function
  | (Some a, Some b) -> Some (func (a,b))
  | _ -> None

let optionMapTriple (func) = function
  | (Some a, Some b, Some c) -> Some (func (a,b,c))
  | _ -> None
