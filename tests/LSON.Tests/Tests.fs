module Tests
open Expecto

[<Tests>]
let tests =
  testList "samples" [
    testCase "symbol value" <| fun _ ->
      Expect.equal "(symbol \"value\")" (LSON.parse "(symbol \"value\")" |> LSON.stringify) "should be able to parse and stringify symbol"
  ]
