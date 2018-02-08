module Tests
open Expecto
let sampleStrings =
  [
    "(symbol \"value\")"
    "(\"value with \\\" \")"
    //, \n, \v, \f, \r
  ]
let testCaseOfSample sample=
  testCase <| sprintf "sample %s" sample <| fun _ ->
    Expect.equal sample (LSON.parse sample |> LSON.stringify) <| sprintf "should be able to parse and stringify %s" sample

[<Tests>]
let tests =
  testList "samples" <|
      (seq {
        for sample in sampleStrings do yield testCaseOfSample sample
      } |> Seq.toList)

