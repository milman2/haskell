{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$white = [\ \t\n\r]

tokens :-

  $white+               ;
  "+"                   { \_ -> Plus }
  "*"                   { \_ -> Times }
  "("                   { \_ -> LParen }
  ")"                   { \_ -> RParen }
  $digit+               { \s -> IntTok (read s) }

{
data Token
  = IntTok Int
  | Plus
  | Times
  | LParen
  | RParen
  deriving (Show)
}
