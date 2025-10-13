{-# LANGUAGE TemplateHaskell #-}

module TH where
import Language.Haskell.TH

squareExpr :: Q Exp
squareExpr = [| \x -> x * x |]

