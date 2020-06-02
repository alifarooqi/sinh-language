{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-

  $white+                       ;
  "--".*                        ;
  var                           { \s -> TokenVar }
  if                            { \s -> TokenIf }
  else                          { \s -> TokenElse }
  true                          { \s -> TokenTrue }
  false                         { \s -> TokenFalse }
  case                          { \s -> TokenCase }
  of                            { \s -> TokenOf }
  raise                         { \s -> TokenRaise }
  try                           { \s -> TokenTry }
  with                          { \s -> TokenWith }
  type                          { \s -> TokenType }
  \|                            { \s -> TokenBar }
  $digit+                       { \s -> TokenInt (read s) }
  \"[^\"]*\"                    { \s -> TokenString (read s) }
  \<\-                          { \s -> TokenAssign }
  \;                            { \s -> TokenSemiColon }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenPlus }
  \-                            { \s -> TokenMinus }
  \*                            { \s -> TokenTimes }
  \/                            { \s -> TokenDiv }
  \^                            { \s -> TokenPow }
  \<                            { \s -> TokenLT }
  \<\=                          { \s -> TokenLE }
  \=\>                          { \s -> TokenCaseArrow }
  \>\=                          { \s -> TokenGE }
  \>                            { \s -> TokenGT }
  \=\=                          { \s -> TokenComp }
  \&\&                          { \s -> TokenAnd }
  \|\|                          { \s -> TokenOr }
  \!                            { \s -> TokenNot }
  \(                            { \s -> TokenLParen }
  \)                            { \s -> TokenRParen }
  \{                            { \s -> TokenLB }
  \}                            { \s -> TokenRB }
  \,                            { \s -> TokenComma }
  \:                            { \s -> TokenColon }
  \-\>                          { \s -> TokenArrow }
  function                      { \s -> TokenFunc }
  mutable                       { \s -> TokenMut }
  Int                           { \s -> TokenTInt }
  String                        { \s -> TokenTString }
  Bool                          { \s -> TokenTBool }
  $alpha [$alpha $digit \_ \']* { \s -> TokenSym s }
  \@                            { \s -> TokenTopLevelFun }
  \.                            { \s -> TokenDot }
  \$                            { \s -> TokenAt }
  



{
-- The token type:
data Token = TokenInt Int
           | TokenString String
           | TokenSym String
           | TokenVar
           | TokenIf
           | TokenElse
           | TokenTrue
           | TokenFalse
           | TokenSemiColon
           | TokenPlus
           | TokenEq
           | TokenMinus
           | TokenTimes
           | TokenDiv
           | TokenPow
           | TokenLT
           | TokenLE
           | TokenGT
           | TokenGE
           | TokenComp
           | TokenAnd
           | TokenOr
           | TokenNot
           | TokenLParen
           | TokenRParen
           | TokenLB
           | TokenRB
           | TokenComma
           | TokenColon
           | TokenFunc
           | TokenTInt
           | TokenTBool
           | TokenTString
           | TokenArrow
           | TokenTopLevelFun
           | TokenCase
           | TokenOf
           | TokenBar
           | TokenCaseArrow
           | TokenRaise
           | TokenTry
           | TokenWith
           | TokenDot
           | TokenType
           | TokenAt
           | TokenMut
           | TokenAssign
           deriving (Eq,Show)

scanTokens = alexScanTokens
}
