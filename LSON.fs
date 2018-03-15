module internal LSON
open System
// https://github.com/AshleyF/FScheme/blob/master/FScheme.fs

type Token =
    | Open | Close
    | Quote | Unquote
    | String of string
    | Symbol of string

let private tokenize source =
    let rec string acc = function
        | '\\' :: '"' :: t -> string (acc + "\"") t // escaped quote becomes quote
        | '\\' :: 'b' :: t -> string (acc + "\b") t // escaped backspace
        | '\\' :: 'f' :: t -> string (acc + "\f") t // escaped formfeed
        | '\\' :: 'n' :: t -> string (acc + "\n") t // escaped newline
        | '\\' :: 'r' :: t -> string (acc + "\r") t // escaped return
        | '\\' :: 't' :: t -> string (acc + "\t") t // escaped tab
        | '\\' :: '\\' :: t -> string (acc + "\\") t // escaped backslash
        | '\\' :: 'v' :: t -> string (acc + "\u000B") t //
        | '"' :: t -> acc, t // closing quote terminates
        | c :: t -> string (acc + (c.ToString())) t // otherwise accumulate chars
        | _ -> failwith "Malformed string."
    let rec comment = function
        | '\r' :: t | '\n' :: t -> t // terminated by line end
        | [] -> [] // or by EOF
        | _ :: t -> comment t
    let rec token acc = function
        | (')' :: _) as t -> acc, t // closing paren terminates
        | w :: t when Char.IsWhiteSpace(w) -> acc, t // whitespace terminates
        | [] -> acc, [] // end of list terminates
        | c :: t -> token (acc + (c.ToString())) t // otherwise accumulate chars
    let rec tokenize' acc = function
        | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t // skip whitespace
        | '(' :: t -> tokenize' (Open :: acc) t
        | ')' :: t -> tokenize' (Close :: acc) t
        | '\'' :: t -> tokenize' (Quote :: acc) t
        | ',' :: t -> tokenize' (Unquote :: acc) t
        | ';' :: t -> comment t |> tokenize' acc // skip over comments
        | '"' :: t -> // start of string
            let s, t' = string "" t
            tokenize' (Token.String(s) :: acc) t'
        | s :: t -> // otherwise start of symbol
            let s, t' = token (s.ToString()) t
            tokenize' (Token.Symbol(s) :: acc) t'
        | [] -> List.rev acc // end of list terminates
    tokenize' [] source

type SExpr =
    | String of string
    | Token of string
    | List of SExpr list

let parse (source:string) : SExpr=
    let map = function
        | Token.String(s) -> SExpr.String(s)
        | Token.Symbol(s) -> SExpr.Token(s)
        | _ -> failwith "Syntax error."
    let rec list f t acc =
        let e, t' = parse' [] t
        parse' (List(f e) :: acc) t'
    and parse' acc = function
        | Open :: t -> list id t acc
        | Close :: t -> (List.rev acc), t
        | Quote :: Open :: t -> list (fun e -> [Token("quote"); List(e)]) t acc
        | Quote :: h :: t -> parse' (List([Token("quote"); map h]) :: acc) t
        | Unquote :: Open :: t -> list (fun e -> [Token("unquote"); List(e)]) t acc
        | Unquote :: h :: t -> parse' (List([Token("unquote"); map h]) :: acc) t
        | h :: t -> parse' ((map h) :: acc) t
        | [] -> (List.rev acc), []
    let result, _ = parse' [] (tokenize ( source.ToCharArray() |> List.ofArray ))
    match result with
    | [v] -> v
    | [] -> failwith "Syntax error (nothing parsed)."
    | _ -> failwith "Syntax error (to many s-expressions)."

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
