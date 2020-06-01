{
module Parser (parseExpr, E (..)) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
}


%name parser
%tokentype { Token }
%error { parseError }
%monad { E } { thenE } { returnE }

%token
    var     { TokenVar }
    id      { TokenSym $$ }
    int     { TokenInt $$ }
    string  { TokenString $$ }
    Int     { TokenTInt }
    Bool    { TokenTBool }
    String  { TokenTString }
    case    { TokenCase }
    of      { TokenOf }
    raise   { TokenRaise }
    try     { TokenTry }
    with    { TokenWith }
    type    { TokenType }
    '<-'    { TokenAssign }
    '|'     { TokenBar }
    '+'     { TokenPlus }
    '-'     { TokenMinus }
    '*'     { TokenTimes }
    '/'     { TokenDiv }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '}'     { TokenRB }
    '{'     { TokenLB }
    ';'     { TokenSemiColon }
    ':'     { TokenColon }
    ','     { TokenComma }
    '='     { TokenEq }
    if      { TokenIf }
    else    { TokenElse }
    true    { TokenTrue }
    false   { TokenFalse }
    '<'     { TokenLT }
    '<='    { TokenLE }
    '=>'    { TokenCaseArrow }
    '>'     { TokenGT }
    '>='    { TokenGE }
    '=='    { TokenComp }
    '&&'    { TokenAnd }
    '!'     { TokenNot }
    '||'    { TokenOr }
    fun     { TokenFunc }
    mut     { TokenMut }
    '->'    { TokenArrow }
    '@'     {TokenTopLevelFun}
    '.'     { TokenDot }
    '$'     { TokenAt }
    

%right ';' else
%left '||'
%left '&&'
%nonassoc '=='
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '/'
%left NEG NOT AT MUTA
%right '->'

%%

Program : Types Functions Exp        { Program $1 $2 $3 }

Functions: Functions Function  { $1 ++ [$2] }
         |                     { [] }

Function : fun typ id  '(' ids ')' '{' Exp '}'   { ($2, $3, Function $5 $8) }

Types : Types Type  { $1 ++ [$2] }
         |          { [] }

Type : type id '=' typ   { ($2, $4) }

ids : ids ',' id ':' typ    { $1 ++ [($3, $5)] }
    | id ':' typ            { [($1, $3)] }
    |                       { [] }

typ : Int           { TInt }
    | Bool          { TBool }
    | String        { TString }
    | typ '->' typ  { TFun $1 $3 }
    | '{' tRcds '}'  { TRcd $2 }
    | '<' tRcds '>'  { TVarnt $2 }
    | id             { TypDecl $1 }

tRcds: tRcds tRcd1  { $1 ++ [$2] }
    | {- empty -}    { [] }

tRcd1 : id ':' typ       { ($1, $3) }
      | id ':' typ ','   { ($1, $3) }

Exp : fun '(' id ':' typ ')' '{' Exp '}'   { Fun ($3, $5) $8 }
    | var id ':' typ '=' Exp ';' Exp       { Decl $2 $4 $6 $8 }
    | id '<-' Exp ';' Exp                  { Assign (Var $1) $3 $5 }
    | if '(' Exp ')' Exp ';' else Exp      { If $3 $5 $8 }
    | Exp '||' Exp                         { Bin Or $1 $3 }
    | Exp '&&' Exp                         { Bin And $1 $3 }
    | Exp '==' Exp                         { Bin EQ $1 $3 }
    | Exp '<' Exp                          { Bin LT $1 $3 }
    | Exp '>' Exp                          { Bin GT $1 $3 }
    | Exp '<=' Exp                         { Bin LE $1 $3 }
    | Exp '>=' Exp                         { Bin GE $1 $3 }
    | Exp '+' Exp                          { Bin Add $1 $3 }
    | Exp '-' Exp                          { Bin Sub $1 $3 }
    | Exp '*' Exp                          { Bin Mult $1 $3 }
    | Exp '/' Exp                          { Bin Div $1 $3 }
    | '-' Exp %prec NEG                    { Unary Neg $2 }
    | '!' Exp %prec NOT                    { Unary Not $2 }
    | '@' id '(' Exps ')'                  { Call $2 $4 }
    | '{' Rcds '}'                         { Rcd $2 }
    | Exp '.' id                           { RcdProj $1 $3 }
    | '<' id '=' Exp ':' typ '>'           { Varnt $2 $4 $6 }
    | App                                  { $1 }
    | case Exp of Cases                    { CaseV $2 $4 }
    | raise Exp                            { Raise $2 }
    | try Exp with Exp                     { Try $2 $4 }
    | '$' Exp %prec AT                     { Access $2 }
    | mut Exp %prec MUTA                   { Mutable $2 }

Cases: Cases Cases1 { $1 ++ [$2] }
     | {- empty -}    { [] }

Cases1 : '|' '<' id '=' id '>' '=>' Exp  { ($3, $5, $8) }

Rcds: Rcds Rcd1  { $1 ++ [$2] }
    | {- empty -}    { [] }

Rcd1 : id '=' Exp   { ($1, $3) }
     | id '=' Exp ','   { ($1, $3) }


App : App '(' Exp ')'                    { CallFC $1 $3 } -- Redundant
    | id '(' Exps ')'                    { Call $1 $3 }
    | '(' Exp ')'                        { $2 }
    | int                                { Lit (IntV $1) }
    | true                               { Lit (BoolV True) }
    | false                              { Lit (BoolV False) }
    | string                             { Lit (StringV $1) }
    | id                                 { Var $1 }

Exps : Exps ',' Exp                   { $1 ++ [$3] }
     | Exp                            { [$1] }
     |                                { [] }

{

data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k = 
   case m of 
      (Ok a) -> k a
      (Failed e) -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k = 
   case m of
      (Ok a) -> Ok a
      (Failed e) -> k e


parseError :: [Token] -> E a
parseError _ = failE $ "Parse error: Unexpected syntax used in the program"

parseExpr = parser . scanTokens

}
