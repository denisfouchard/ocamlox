(* LOWEST TO HIGHEST
Name	        Operators	    Associates
---------------------------------------
Equality	    == !=	        Left
Comparison	  > >= < <=	    Left
Term	        - +	          Left
Factor	      / *	          Left
Unary	        ! -	          Right


program        → declaration* EOF ;

declaration    → varDecl
               | statement ;

statement      → exprStmt
              | ifStmt
              | whileStmt
              | forStmt
              | printStmt
              | block ;

ifStmt         → "if" "(" expression ")" statement
              ( "else" statement )? ;

whileStmt      -> "while" "(" expression ")" statement

block          → "{" declaration* "}" ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";"

expression     → assignment ;
assignment     → IDENTIFIER "=" assignment
               | logic_or ;
logic_or       → logic_and ( "or" logic_and )* ;
logic_and      → equality ( "and" equality )* ;

equality       → comparison ( ( "!=" of ast * ast | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary| primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;

*)
open Scanner


type tok_l = Scanner.token list
[@@deriving show]

type ast =
  | EMPTY
  (*Binary operators*)
  | IS_EQAL of ast * ast
  | IS_NEQ of ast * ast
  | LT of ast * ast
  | LEQ of ast * ast
  | GT of ast * ast
  | GEQ of ast * ast
  | ADD of ast * ast
  | SUB of ast * ast
  | MULT of ast * ast
  | DIV of ast * ast

  (*Logical operations*)
  | OR of ast * ast
  | AND of ast * ast

  (*Unary operators*)
  |NOT of ast
  |NEG of ast

 (*Literals*)
 | NUMBER_VALUE of float
 | STRING_VALUE of string
 | BOOL_TRUE
 | BOOL_FALSE
 | NIL_VALUE

 (**DECLARATIONS**)
 (*Statements*)
 | StatementSequence of ast list

 | Statement of ast
 | ExpressionStatement of ast
 | PrintStatement of ast
 | IfStatement of {condition:ast;
                    then_branch:ast;
                    else_branch: ast option
                  }

 | WhileStatement of {condition:ast;
                     loop:ast}
 | ForStatement of {var_decl:ast;
                    loop_condition:ast;
                    incr:ast;
                    loop:ast
                  }

 | VariableDeclaration of {name:string; value:ast}
 | VariableAccess of string
 | VariableMutation of {name:string; value:ast}

 | Block of ast list
[@@deriving show]

let is_primary (token:token) =
  match token.token_type with
    | NIL -> true
    | TRUE -> true
    | FALSE -> true
    | VAR -> true
    | IDENTIFIER -> true
    | STRING -> true
    | _ -> false

let is_compare (token:token) =
  match token.token_type with
    | GREATER -> true
    | GREATER_EQUAL -> true
    | LESS -> true
    | LESS_EQUAL -> true
    | _ -> false
