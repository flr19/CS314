{
open Parser
}

let alph = ['a'-'z' 'A'-'Z']
let rest = ['a'-'z' 'A'-'Z' '0'-'9' '\'']*
let number = ['0'-'9'] ['0'-'9']*
rule token = parse
  | [' ' '\t' '\n'] { token lexbuf } (* skip whitespace *)
  | "true"  { TRUE }
  | "false" { FALSE }
  | "skip"  { SKIP }
  | "if"    { IF }
  | "else"  { ELSE }
  | "while" { WHILE }
  | "for"   { FOR }
  | "print" { PRINT }
  | "input" { INPUT }
  | "int"  { INT }
  | "bool" { BOOL }
  | "lambda" { LAMBDA }
  | "fun"  { FUN }
  | alph rest as id { VAR id }
  | number as number { NUMBER (int_of_string number) }
  | '&'    { AND }
  | '|'    { OR }
  | '~'    { NOT }
  | '+'    { PLUS }
  | '-'    { MINUS }
  | '*'    { TIMES }
  | '/'    { DIV }
  | '%'    { MOD }
  | '='    { EQ }
  | "<="   { LEQ }
  | '<'    { LT }
  | ">="   { GEQ }
  | '>'    { GT }
  | '('    { LPAREN }
  | ')'    { RPAREN }
  | '{'    { LBRACE }
  | '}'    { RBRACE }
  | ":="   { ASSG }
  | ';'    { COMP }
  | eof    { EOL }
  | "->"   { ARROW }
