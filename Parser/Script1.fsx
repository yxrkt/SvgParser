open Svg

let (|Percent|) value = 100.0 * value

match "7".ToCharArray () |> Array.toList with
| Parser.Wsp _ -> blegh