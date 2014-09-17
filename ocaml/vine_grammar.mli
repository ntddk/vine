type token =
  | ID of (string)
  | INT of (Int64.t)
  | STRING of (string)
  | COMMENT of (string)
  | TYP of (Vine.typ)
  | LPAREN
  | RPAREN
  | SEMI
  | EOF
  | LCURLY
  | RCURLY
  | COLON
  | LSQUARE
  | RSQUARE
  | COMMA
  | CJMP
  | NAME
  | JMP
  | CAST
  | INIT
  | VAR
  | LET
  | IN
  | TRUE
  | FALSE
  | LABEL
  | ATTR
  | CALL
  | ASSERT
  | HALT
  | SPECIAL
  | UNKNOWN
  | STATE
  | TVOID
  | RETURN
  | EXTERN
  | PLUS
  | MINUS
  | DIVIDE
  | MOD
  | SMOD
  | TIMES
  | SDIVIDE
  | LSHIFT
  | RSHIFT
  | ARSHIFT
  | XOR
  | NEQ
  | SLT
  | SLE
  | AND
  | OR
  | EQUAL
  | LT
  | LE
  | NOT
  | ASSIGN
  | GT
  | GE
  | SGT
  | SGE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Vine.program
val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Vine.exp 
