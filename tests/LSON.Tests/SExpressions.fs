module SExpressions
open Expecto
open LSON
type internal E=SExpr

type Foo={ alpha:string; beta:string}

type Bar={ gamma:string; foo:Foo}

type Baz={ alpha:string; beta:string}


type Qux =
  |Ena of string
  |Dio of Foo
  |Trea of Bar
  |Tessera of Baz

module internal Z=
  module Foo=
   let serialize (this:Foo)=
      E.List [
                E.Token "alpha"; E.String this.alpha
                E.Token "beta"; E.String this.beta
             ]
   let deSerialize(v:E) : Foo option=
      match v with
      | E.List [
                E.Token "alpha"; E.String alpha
                E.Token "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None

  module Bar=
    let serialize this=
      E.List [
                E.Token "gamma"; E.String this.gamma
                E.Token "foo"; Foo.serialize this.foo
             ]
    let deSerialize(v:E)=
      match v with
      | E.List [
                E.Token "gamma"; E.String gamma
                E.Token "foo"; foo
               ] ->
                 Foo.deSerialize foo |> Option.map (fun foo-> { gamma=gamma; foo= foo})
      | _ -> None

  module Baz=
    let serialize this=
      E.List [
                E.Token "alpha"; E.String this.alpha
                E.Token "beta"; E.String this.beta
             ]
    let deSerialize(v:E)=
      match v with
      | E.List [
                E.Token "alpha"; E.String alpha
                E.Token "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None
  module Qux=
    let serialize (this:Qux)=
      match this with
      | Ena s-> E.List [ E.Token "Ena"; E.String s ]
      | Dio foo -> E.List [ E.Token "Dio"; Foo.serialize foo ]
      | Trea bar -> E.List [ E.Token "Trea"; Bar.serialize bar ]
      | Tessera baz -> E.List [ E.Token "Tessera"; Baz.serialize baz ]
    let deSerialize(v:E)=
      match v with
      | E.List [ E.Token "Ena"; E.String s ] -> Some <| Ena s
      | E.List [ E.Token "Dio"; foo ] -> Foo.deSerialize foo |> Option.map Dio
      | E.List [ E.Token "Trea"; bar ] -> Bar.deSerialize bar |> Option.map Trea
      | E.List [ E.Token "Tessera"; baz ] -> Baz.deSerialize baz |> Option.map Tessera
      | _ -> None


[<Tests>]
let tests =
  testList "S-Expressions" [
    testProperty "Can serialize and deserialize structure Union" <|
      fun (x :Qux) ->
          (Some x) = ( Z.Qux.serialize x |> Z.Qux.deSerialize )
    testProperty "Can serialize and deserialize structure Record" <|
      fun (x :Baz) ->
          (Some x) = ( Z.Baz.serialize x |> Z.Baz.deSerialize )
    testCase "Can parse more complex example"  <| fun _ ->
      let c= """(((S) (NP VP))
       ((VP) (V))
       ((VP) (V NP))
       ((V) died)
       ((V) employed)
       ((NP) nurses)
       ((NP) patients)
       ((NP) Medicenter)
       ((NP) "Dr Chan"))"""

      LSON.parse c |> ignore
    testCase "Can parse (\"S\")"  <| fun _ ->
      let c= "(\"S\")"
      let res = LSON.parse c 
      Expect.equal (E.List [E.String "S"]) res "should be able to interpret (\"S\")"

    testCase "Can parse (S)"  <| fun _ ->
      let c= "(S)"
      let res = LSON.parse c 
      Expect.equal (E.List [E.Token "S"]) res "should be able to interpret (S)"

    testCase "Can parse empty ()"  <| fun _ ->
      let c= "()"
      let res = LSON.parse c 
      Expect.equal (E.List []) res "should be able to interpret empty ()"
    testCase "Can parse empty string"  <| fun _ ->
      let c= "\"\""
      let res = LSON.parse c 
      Expect.equal (E.String "") res "should be able to interpret empty string"
    testCase "Can parse identifier"  <| fun _ ->
      let c= "identifier"
      let res = LSON.parse c 
      Expect.equal (E.Token "identifier") res "should be able to interpret identifier"
  ]

