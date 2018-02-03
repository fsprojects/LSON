module SExpressions
open Expecto
open LSON
type E=SExpr

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
                E.String "alpha"; E.String this.alpha
                E.String "beta"; E.String this.beta
             ]
   let deSerialize(v:E) : Foo option=
      match v with
      | E.List [
                E.String "alpha"; E.String alpha
                E.String "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None

  module Bar=
    let serialize this=
      E.List [
                E.String "gamma"; E.String this.gamma
                E.String "foo"; Foo.serialize this.foo
             ]
    let deSerialize(v:E)=
      match v with
      | E.List [
                E.String "gamma"; E.String gamma
                E.String "foo"; foo
               ] ->
                 Foo.deSerialize foo |> Option.map (fun foo-> { gamma=gamma; foo= foo})
      | _ -> None

  module Baz=
    let serialize this=
      E.List [
                E.String "alpha"; E.String this.alpha
                E.String "beta"; E.String this.beta
             ]
    let deSerialize(v:E)=
      match v with
      | E.List [
                E.String "alpha"; E.String alpha
                E.String "beta"; E.String beta
               ] -> Some { alpha=alpha; beta=beta }
      | _ -> None
  module Qux=
    let serialize (this:Qux)=
      match this with
      | Ena s-> E.List [ E.String "Ena"; E.String s ]
      | Dio foo -> E.List [ E.String "Dio"; Foo.serialize foo ]
      | Trea bar -> E.List [ E.String "Trea"; Bar.serialize bar ]
      | Tessera baz -> E.List [ E.String "Tessera"; Baz.serialize baz ]
    let deSerialize(v:E)=
      match v with
      | E.List [ E.String "Ena"; E.String s ] -> Some <| Ena s
      | E.List [ E.String "Dio"; foo ] -> Foo.deSerialize foo |> Option.map Dio
      | E.List [ E.String "Trea"; bar ] -> Bar.deSerialize bar |> Option.map Trea
      | E.List [ E.String "Tessera"; baz ] -> Baz.deSerialize baz |> Option.map Tessera
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
  ]
