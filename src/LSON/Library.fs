module LSON
open FParsec

type SExpr =
  | String of System.String
  | List of SExpr list

let str s = pstring s

let stringLiteral =
  let escape =  anyOf "\"\\/bfnrt"
                |>> function
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c   -> string c // every other char is mapped to itself

  let unicodeEscape =
      /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
      let hex2int c = (int c &&& 15) + (int c >>> 6)*9

      str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
          (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
          |> char |> string
      )

  let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
  let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

  between (str "\"") (str "\"")
          (stringsSepBy normalCharSnippet escapedCharSnippet)

let sString = stringLiteral |>> SExpr.String
let ws = spaces // skips any whitespace
let sValue, sValueRef = createParserForwardedToRef<SExpr, unit>()
let sList =
  between (str "[") (str "]")
          (ws >>. sepBy (sValue .>> ws) ws |>> SExpr.List)
do sValueRef := choice [
                        sString
                        sList
                       ]
let sExpr = ws >>. sValue .>> ws .>> eof
let parse str =run sExpr str
let rec stringify (expr:SExpr) :string=
  match expr with
  | List ls -> let innerStrings = List.map stringify ls
               sprintf "(%s)" <| String.concat " " innerStrings
  | String s -> s
