{
module Parser where
import Lexer
}

%name parseExpr
%tokentype { Token }
%error { parseError }

%token
  IntTok { IntTok _ }
  '+'    { Plus }
  '*'    { Times }
  '('    { LParen }
  ')'    { RParen }

%%

Expr
  : Term '+' Expr     { Add $1 $3 }
  | Term              { $1 }

Term
  : Factor '*' Term   { Mul $1 $3 }
  | Factor            { $1 }

Factor
  : IntTok            { Num (extractInt $1) }
  | '(' Expr ')'      { $2 }

{
data Expr
  = Num Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Show)

extractInt :: Token -> Int
extractInt (IntTok n) = n
extractInt _ = error "Expected IntTok"

parseError :: [Token] -> a
parseError _ = error "Parse error"
}
