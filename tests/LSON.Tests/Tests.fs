module Tests
open Expecto
open LSON
(*
[<Fact>]
let ``symbol value`` () =

[<Fact>]
let ``X`` () =
  Assert.Equal("(symbol 1 2 3)", S.parse "(symbol 1 2 3)" |> S.printList)
*)

[<Tests>]
let tests =
  testList "samples" [
    testCase "symbol value" <| fun _ ->
      Expect.equal "(symbol \"value\")" (LSON.parse "(symbol \"value\")" |> LSON.stringify) "should be able to parse and stringify symbol"
  ]
