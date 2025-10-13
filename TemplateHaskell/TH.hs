{-# LANGUAGE TemplateHaskell #-}

module TH where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

squareExpr :: Q Exp
squareExpr = [| \x -> x * x |]

genHello :: Q [Dec]
genHello = [d| hello = putStrLn "Hello from TH!" |]

-- 컴파일 타임에 타입 정보를 조회하고 문자열로 변환
inspectType :: Name -> Q Exp
inspectType name = do
  info <- reify name
  let infoStr = show info
  [| putStrLn infoStr |]

-- QuasiQuoter: 사용자 정의 문법 확장
myQuote :: QuasiQuoter
myQuote = QuasiQuoter
  { quoteExp = \s -> [| "You said: " ++ s |]
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

-- 간단한 Show 인스턴스 자동 생성
deriveShow :: Name -> Q [Dec]
deriveShow name = [d| instance Show $(conT name) where show _ = "Color" |]

-- 개선된 버전: 생성자별로 다른 문자열 생성
deriveShowSmart :: Name -> Q [Dec]
deriveShowSmart name = do
  TyConI (DataD _ _ _ _ constructors _) <- reify name
  let instanceD = InstanceD Nothing [] (AppT (ConT ''Show) (ConT name))
                    [FunD 'show (map makeClause constructors)]
  return [instanceD]
  where
    makeClause (NormalC conName _) = 
      Clause [ConP conName [] []] (NormalB (LitE (StringL (nameBase conName)))) []
    makeClause _ = error "Only normal constructors supported"

