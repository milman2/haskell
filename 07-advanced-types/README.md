# 7단계: 고급 타입 시스템

## 학습 목표
- GADT (Generalized Algebraic Data Types) 이해
- 타입 패밀리 (Type Families) 활용
- 고급 타입 클래스와 함수 의존성
- 타입 레벨 프로그래밍
- 타입 안전한 DSL 구현

## 학습 내용

### 1. GADT (Generalized Algebraic Data Types)
- 일반화된 대수 데이터 타입
- 타입 매개변수를 통한 타입 안전성
- 패턴 매칭과 타입 추론

### 2. 타입 패밀리 (Type Families)
- 데이터 타입 패밀리
- 타입 시그니처 패밀리
- 연관 타입

### 3. 고급 타입 클래스
- 함수 의존성 (Functional Dependencies)
- 다중 매개변수 타입 클래스
- 타입 클래스 인스턴스 해결

### 4. 타입 레벨 프로그래밍
- 타입 레벨 자연수
- 타입 레벨 리스트
- 타입 레벨 함수

### 5. 타입 안전한 DSL
- 임베디드 도메인 특화 언어
- 타입 시스템을 활용한 제약 조건

## 프로젝트: 타입 안전한 DSL과 고급 데이터 구조

### 구현할 기능
1. 타입 안전한 벡터 라이브러리
2. SQL 쿼리 DSL
3. 상태 머신 DSL
4. 타입 레벨 계산기

### 예제 코드

#### TypeSafeVector.hs (타입 안전한 벡터)
```haskell
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

module TypeSafeVector where

import GHC.TypeLits

-- 타입 레벨 자연수
data Nat = Z | S Nat

-- 타입 안전한 벡터
data Vector (n :: Nat) a where
    VNil :: Vector 'Z a
    VCons :: a -> Vector n a -> Vector ('S n) a

-- 벡터 길이 계산
type family Length (v :: Vector n a) :: Nat where
    Length ('VNil) = 'Z
    Length ('VCons _ xs) = 'S (Length xs)

-- 벡터 연결
append :: Vector n a -> Vector m a -> Vector (Add n m) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (append xs ys)

-- 타입 레벨 덧셈
type family Add (n :: Nat) (m :: Nat) :: Nat where
    Add 'Z m = m
    Add ('S n) m = 'S (Add n m)

-- 벡터 인덱싱
index :: (KnownNat n, KnownNat i, i < n) => Vector n a -> Proxy i -> a
index (VCons x _) Proxy = x
index (VCons _ xs) Proxy = index xs Proxy

-- 벡터 맵
vmap :: (a -> b) -> Vector n a -> Vector n b
vmap _ VNil = VNil
vmap f (VCons x xs) = VCons (f x) (vmap f xs)

-- 벡터 폴드
vfoldr :: (a -> b -> b) -> b -> Vector n a -> b
vfoldr _ acc VNil = acc
vfoldr f acc (VCons x xs) = f x (vfoldr f acc xs)
```

#### SQLDSL.hs (SQL 쿼리 DSL)
```haskell
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies, FlexibleInstances #-}

module SQLDSL where

-- 데이터베이스 스키마 정의
data Schema = Schema [Table]

data Table = Table String [Column]

data Column = Column String Type

data Type = IntType | StringType | BoolType

-- 테이블 타입 정의
data UserTable = UserTable
data ProductTable = ProductTable
data OrderTable = OrderTable

-- 컬럼 타입 정의
data UserId = UserId
data UserName = UserName
data UserEmail = UserEmail
data ProductId = ProductId
data ProductName = ProductName
data ProductPrice = ProductPrice

-- SQL 표현식
data Expr a where
    Column :: String -> Expr a
    Literal :: a -> Expr a
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or :: Expr Bool -> Expr Bool -> Expr Bool
    Plus :: Num a => Expr a -> Expr a -> Expr a
    Times :: Num a => Expr a -> Expr a -> Expr a

-- SQL 쿼리
data Query a where
    Select :: [Expr b] -> Query [b]
    From :: Query a -> String -> Query a
    Where :: Query a -> Expr Bool -> Query a
    Join :: Query a -> Query b -> Expr Bool -> Query (a, b)
    GroupBy :: Query a -> [Expr b] -> Query [a]
    OrderBy :: Query a -> [Expr b] -> Query a

-- 쿼리 빌더
select :: [Expr a] -> Query [a]
select exprs = Select exprs

from :: Query a -> String -> Query a
from query table = From query table

where_ :: Query a -> Expr Bool -> Query a
where_ query condition = Where query condition

-- 예제 쿼리
exampleQuery :: Query [String]
exampleQuery = 
    select [Column "name", Column "email"]
    `from` "users"
    `where_` (Column "age" `Eq` Literal 25)
```

#### StateMachineDSL.hs (상태 머신 DSL)
```haskell
{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeFamilies #-}

module StateMachineDSL where

-- 상태 정의
data State = Idle | Running | Paused | Stopped

-- 이벤트 정의
data Event = Start | Pause | Resume | Stop

-- 상태 전환 규칙
data Transition where
    Transition :: State -> Event -> State -> Transition

-- 상태 머신 정의
data StateMachine = StateMachine State [Transition]

-- 상태 머신 실행
data StateMachineResult a where
    Success :: a -> StateMachineResult a
    InvalidTransition :: State -> Event -> StateMachineResult a
    Error :: String -> StateMachineResult a

-- 상태 머신 실행 함수
execute :: StateMachine -> Event -> StateMachineResult StateMachine
execute (StateMachine currentState transitions) event = 
    case findTransition currentState event transitions of
        Just newState -> Success (StateMachine newState transitions)
        Nothing -> InvalidTransition currentState event

findTransition :: State -> Event -> [Transition] -> Maybe State
findTransition state event [] = Nothing
findTransition state event (Transition s e newState : rest)
    | s == state && e == event = Just newState
    | otherwise = findTransition state event rest

-- 타입 안전한 상태 머신 빌더
class StateMachineBuilder a where
    build :: a -> StateMachine

-- 상태 머신 정의 예제
playerStateMachine :: StateMachine
playerStateMachine = StateMachine Idle
    [ Transition Idle Start Running
    , Transition Running Pause Paused
    , Transition Paused Resume Running
    , Transition Running Stop Stopped
    , Transition Paused Stop Stopped
    ]
```

#### TypeLevelCalculator.hs (타입 레벨 계산기)
```haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}

module TypeLevelCalculator where

import GHC.TypeLits

-- 타입 레벨 자연수 연산
type family Add (a :: Nat) (b :: Nat) :: Nat where
    Add 0 b = b
    Add a b = Add (a - 1) (b + 1)

type family Multiply (a :: Nat) (b :: Nat) :: Nat where
    Multiply 0 b = 0
    Multiply a b = Add b (Multiply (a - 1) b)

type family Factorial (n :: Nat) :: Nat where
    Factorial 0 = 1
    Factorial n = Multiply n (Factorial (n - 1))

-- 타입 레벨 리스트
data HList (xs :: [*]) where
    HNil :: HList '[]
    HCons :: x -> HList xs -> HList (x ': xs)

-- 타입 레벨 리스트 연산
type family Length (xs :: [*]) :: Nat where
    Length '[] = 0
    Length (x ': xs) = 1 + Length xs

type family Append (xs :: [*]) (ys :: [*]) :: [*] where
    Append '[] ys = ys
    Append (x ': xs) ys = x ': Append xs ys

-- 타입 레벨 조건부
type family If (cond :: Bool) (a :: k) (b :: k) :: k where
    If 'True a b = a
    If 'False a b = b

-- 타입 레벨 비교
type family LessThan (a :: Nat) (b :: Nat) :: Bool where
    LessThan 0 0 = 'False
    LessThan 0 b = 'True
    LessThan a 0 = 'False
    LessThan a b = LessThan (a - 1) (b - 1)

-- 타입 안전한 벡터 (길이 정보 포함)
data Vec (n :: Nat) a where
    VNil :: Vec 0 a
    VCons :: a -> Vec n a -> Vec (n + 1) a

-- 벡터 인덱싱 (타입 안전)
index :: (KnownNat i, i < n) => Vec n a -> Proxy i -> a
index (VCons x _) Proxy = x
index (VCons _ xs) Proxy = index xs Proxy

-- 벡터 연결 (타입 안전)
append :: Vec n a -> Vec m a -> Vec (n + m) a
append VNil ys = ys
append (VCons x xs) ys = VCons x (append xs ys)
```

## 연습 문제
1. 타입 안전한 행렬 라이브러리를 구현하세요
2. 타입 레벨 피보나치 수열을 구현하세요
3. 타입 안전한 JSON 스키마 검증기를 만들어보세요
4. 타입 레벨 정렬 알고리즘을 구현하세요

## 고급 연습 문제
1. 타입 안전한 데이터베이스 쿼리 빌더를 만들어보세요
2. 타입 레벨 컴파일러를 구현해보세요
3. 의존성 주입 프레임워크를 타입 시스템으로 구현해보세요

## 테스트 방법
```bash
# GHC 확장 활성화
ghc -XGADTs -XDataKinds -XTypeFamilies TypeSafeVector.hs

# 타입 체크
ghc -fno-code TypeSafeVector.hs
```

## 다음 단계
8단계에서는 동시성과 병렬성에 대해 학습합니다.
