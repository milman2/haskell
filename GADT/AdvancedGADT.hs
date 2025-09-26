{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module AdvancedGADT where

import Data.Text (Text, pack, unpack)
import qualified Data.Text as T

-- 1. 고급 GADT: 타입 레벨 프로그래밍

-- 자연수 타입 (Peano 수)
data Nat = Z | S Nat

-- 타입 레벨 자연수
data SNat (n :: Nat) where
    SZ :: SNat 'Z
    SS :: SNat n -> SNat ('S n)

-- 벡터 타입 (길이 정보를 타입에 포함)
data Vec (n :: Nat) a where
    VNil  :: Vec 'Z a
    VCons :: a -> Vec n a -> Vec ('S n) a

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)

-- 2. 타입 안전한 벡터 연산들

-- 벡터 길이 (런타임에서도 타입 정보 보존)
vlength :: Vec n a -> SNat n
vlength VNil = SZ
vlength (VCons _ xs) = SS (vlength xs)

-- 벡터 인덱싱 (범위 체크가 타입 레벨에서)
vindex :: SNat n -> Vec ('S n) a -> a
vindex SZ (VCons x _) = x
vindex (SS n) (VCons _ xs) = vindex n xs

-- 벡터 연결 (길이 정보 보존)
vappend :: Vec m a -> Vec n a -> Vec (Plus m n) a
vappend VNil ys = ys
vappend (VCons x xs) ys = VCons x (vappend xs ys)

-- 타입 레벨 덧셈
type family Plus (m :: Nat) (n :: Nat) :: Nat where
    Plus 'Z n = n
    Plus ('S m) n = 'S (Plus m n)

-- 3. DSL: 타입 안전한 행렬 연산

-- 행렬 타입 (행과 열의 크기를 타입에 포함)
data Matrix (m :: Nat) (n :: Nat) a where
    Matrix :: Vec m (Vec n a) -> Matrix m n a

deriving instance (Show a, Show (Vec n a)) => Show (Matrix m n a)

-- 행렬 생성
matrix :: Vec m (Vec n a) -> Matrix m n a
matrix = Matrix

-- 간단한 벡터 연산들
vhead :: Vec ('S n) a -> a
vhead (VCons x _) = x

vtail :: Vec ('S n) a -> Vec n a
vtail (VCons _ xs) = xs

-- 벡터를 리스트로 변환
vtoList :: Vec n a -> [a]
vtoList VNil = []
vtoList (VCons x xs) = x : vtoList xs

-- 리스트를 벡터로 변환 (길이 정보는 런타임에서 확인)
vfromList :: [a] -> Maybe (SomeVec a)
vfromList [] = Just (SomeVec VNil)
vfromList (x:xs) = case vfromList xs of
    Just (SomeVec vec) -> Just (SomeVec (VCons x vec))
    Nothing -> Nothing

-- 타입 지우기
data SomeVec a where
    SomeVec :: Vec n a -> SomeVec a

-- 4. 타입 안전한 예제들
exampleVec1 :: Vec ('S ('S 'Z)) Int
exampleVec1 = VCons 1 (VCons 2 VNil)

exampleVec2 :: Vec ('S ('S 'Z)) Int
exampleVec2 = VCons 3 (VCons 4 VNil)

exampleMatrix1 :: Matrix ('S ('S 'Z)) ('S ('S 'Z)) Int
exampleMatrix1 = Matrix (VCons exampleVec1 (VCons exampleVec2 VNil))

-- 5. 테스트 함수
testAdvancedGADT :: IO ()
testAdvancedGADT = do
    putStrLn "=== Advanced GADT Examples ==="
    
    putStrLn "벡터 예제:"
    putStrLn $ "벡터1: " ++ show exampleVec1
    putStrLn $ "벡터2: " ++ show exampleVec2
    
    putStrLn $ "벡터1의 첫 번째 원소: " ++ show (vhead exampleVec1)
    putStrLn $ "벡터1을 리스트로: " ++ show (vtoList exampleVec1)
    
    let combined = vappend exampleVec1 exampleVec2
    putStrLn $ "연결된 벡터: " ++ show combined
    
    putStrLn "\n행렬 예제:"
    putStrLn $ "행렬: " ++ show exampleMatrix1
    
    putStrLn "\nGADT의 고급 활용:"
    putStrLn "- 타입 레벨 프로그래밍"
    putStrLn "- 컴파일 타임 크기 검증"
    putStrLn "- 런타임 오류 방지"
    putStrLn "- 복잡한 타입 불변식 보장"
