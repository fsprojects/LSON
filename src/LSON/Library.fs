module LSON
open FParsec

type SExpr =
  | Token of System.String
  | String of System.String
  | List of SExpr list

let str s = pstring s

let stringLiteral =
  let escape =  anyOf "\"\\/abfnrtv"
                |>> function
                    | 'a' -> "\u0007"
                    | 'b' -> "\b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | 'v' -> "\u000B"
                    | c   -> string c // every other char is mapped to itself

  let unicodeEscape =
      /// converts a hex char ([0-9a-fA-F]) to its integer number (0-15)
      let hex2int c = (int c &&& 15) + (int c >>> 6)*9

      str "x" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
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
let ws1 = skipMany1Satisfy System.Char.IsWhiteSpace

let sToken =
  let symbol = "!$%&|*+-/:<=>?@^_~#"
  let isIdentifierStart = fun c -> isAsciiLetter c || isAnyOf symbol c
  let isIdentifierCont  = fun c -> isAsciiLetter c || isDigit c || isAnyOf symbol c
  let identifierString = many1Satisfy2 isIdentifierStart isIdentifierCont
  identifierString |>> Token

let sList =
  between (skipChar '(') (skipChar ')')
          (sepBy sValue ws1 |>> SExpr.List)
do sValueRef := choice [
                        sString
                        sToken
                        sList
                       ]
let sExpr = ws >>. sValue .>> ws .>> eof
let parse str =
  let r= run sExpr str
  match r with
  | Success (v,_,_)->v
  | Failure (str,err,_)-> failwithf "%s %A" str err
type internal ME= System.Text.RegularExpressions.MatchEvaluator
type internal Regex= System.Text.RegularExpressions.Regex
let rec stringify (expr:SExpr) :string=
  let escapeChars = Regex("[\"]")
  let escape s = escapeChars.Replace(s, ME(fun m->sprintf "\\%s" m.Value))

  match expr with
  | Token t -> t
  | List ls -> let innerStrings = List.map stringify ls
               sprintf "(%s)" <| String.concat " " innerStrings
  | String s -> sprintf "\"%s\"" <| escape s
