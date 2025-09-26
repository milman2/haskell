# GADT (Generalized Algebraic Data Types) 예제

이 프로젝트는 GADT를 익히고 DSL에서 어떻게 활용되는지 보여주는 예제들입니다.

## 📁 프로젝트 구조

```
GADT/
├── BasicGADT.hs        # 기본 GADT 예제
├── DSLGADT.hs          # DSL에서의 GADT 활용
├── AdvancedGADT.hs     # 고급 GADT (타입 레벨 프로그래밍)
├── ConfigGADT.hs       # 설정 DSL에서의 GADT
├── Main.hs             # 메인 실행 파일
├── GADT.cabal          # Cabal 설정
└── README.md           # 이 파일
```

## 🚀 빌드 및 실행

### 방법 1: GHC 직접 사용

```bash
cd /home/milman2/haskell/GADT
ghc -o GADT Main.hs BasicGADT.hs DSLGADT.hs AdvancedGADT.hs ConfigGADT.hs
./GADT
```

### 방법 2: Cabal 사용

```bash
cd /home/milman2/haskell/GADT
cabal build
cabal run GADT
```

### 방법 3: Stack 사용

```bash
cd /home/milman2/haskell/GADT
stack build
stack exec GADT
```

## 📚 GADT 예제 설명

### 1. BasicGADT.hs - 기본 GADT

**주요 개념:**
- GADT의 기본 문법과 사용법
- 타입 안전한 표현식 시스템
- 패턴 매칭 시 타입 정보 보존

**핵심 코드:**
```haskell
data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    Eq      :: Eq a => Expr a -> Expr a -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a
```

**장점:**
- 타입 안전성: 잘못된 타입 조합은 컴파일 시점에 감지
- 패턴 매칭 시 타입 정보 보존
- 평가 함수에서 타입 캐스팅 불필요

### 2. DSLGADT.hs - DSL에서의 GADT

**주요 개념:**
- 타입 안전한 SQL 쿼리 DSL
- 쿼리 타입에 따른 결과 타입 자동 결정
- 타입 패밀리와 GADT의 조합

**핵심 코드:**
```haskell
data SQLQuery (q :: QueryType) where
    SelectQuery :: [Text] -> Text -> Maybe Condition -> SQLQuery Select
    InsertQuery :: Text -> [(Text, Value)] -> SQLQuery Insert
    UpdateQuery :: Text -> [(Text, Value)] -> Maybe Condition -> SQLQuery Update
    DeleteQuery :: Text -> Maybe Condition -> SQLQuery Delete
```

**장점:**
- 쿼리 타입에 따라 결과 타입이 자동으로 결정
- 잘못된 쿼리 조합은 컴파일 시점에 감지
- 타입 안전한 쿼리 실행

### 3. AdvancedGADT.hs - 고급 GADT

**주요 개념:**
- 타입 레벨 프로그래밍
- 벡터와 행렬의 크기 정보를 타입에 포함
- 컴파일 타임 크기 검증

**핵심 코드:**
```haskell
data Vec (n :: Nat) a where
    VNil  :: Vec 'Z a
    VCons :: a -> Vec n a -> Vec ('S n) a

data Matrix (m :: Nat) (n :: Nat) a where
    Matrix :: Vec m (Vec n a) -> Matrix m n a
```

**장점:**
- 타입 레벨 프로그래밍
- 컴파일 타임 크기 검증
- 런타임 오류 방지
- 복잡한 타입 불변식 보장

### 4. ConfigGADT.hs - 설정 DSL

**주요 개념:**
- 타입 안전한 설정 관리
- 중첩된 설정 구조의 타입 안전성
- 설정 검증의 타입 안전성

**핵심 코드:**
```haskell
data ConfigValue (t :: ConfigType) where
    StringConfig :: Text -> ConfigValue String
    IntConfig :: Int -> ConfigValue Int
    BoolConfig :: Bool -> ConfigValue Bool
    ListConfig :: [Text] -> ConfigValue List
    SectionConfig :: Map Text SomeConfigValue -> ConfigValue Section
```

**장점:**
- 설정 타입에 따른 타입 안전성
- 잘못된 타입 조회는 컴파일 시점에 감지
- 중첩된 설정 구조의 타입 안전성
- 설정 검증의 타입 안전성

## 🔧 주요 기술적 요소

### 1. GADT 문법

```haskell
{-# LANGUAGE GADTs #-}

data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
```

### 2. 타입 레벨 프로그래밍

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data Nat = Z | S Nat

data Vec (n :: Nat) a where
    VNil  :: Vec 'Z a
    VCons :: a -> Vec n a -> Vec ('S n) a
```

### 3. 타입 패밀리

```haskell
{-# LANGUAGE TypeFamilies #-}

type family QueryResult (q :: QueryType) where
    QueryResult Select = [Text]
    QueryResult Insert = Int
    QueryResult Update = Int
    QueryResult Delete = Int
```

## 🎯 GADT의 장점

### 1. 타입 안전성
- 잘못된 타입 조합은 컴파일 시점에 감지
- 런타임 타입 오류 방지

### 2. 타입 정보 보존
- 패턴 매칭 시 타입 정보가 보존됨
- 타입 캐스팅 불필요

### 3. DSL 구현에 유용
- 도메인 특화 언어의 타입 안전성 보장
- 복잡한 타입 불변식 표현 가능

### 4. 컴파일 타임 검증
- 타입 레벨 프로그래밍으로 컴파일 타임 검증
- 런타임 오류 방지

## 🚀 실행 결과 예시

```
=== Basic GADT Examples ===
5 + 3 = 8
5 == 3 = False
if True then 10 else 20 = 10

=== DSL with GADT Examples ===
Generated queries:
SELECT: SelectQuery ["name","age"] "users" (Just (Gt "age" (IntValue 18)))
INSERT: InsertQuery "users" [("name",StringValue "John"),("age",IntValue 25)]
UPDATE: UpdateQuery "users" [("age",IntValue 26)]
DELETE: DeleteQuery "users" Nothing

=== Advanced GADT Examples ===
벡터 예제:
벡터1: VCons 1 (VCons 2 VNil)
벡터2: VCons 3 (VCons 4 VNil)
연결된 벡터: VCons 1 (VCons 2 (VCons 3 (VCons 4 VNil)))

=== Config DSL with GADT Examples ===
생성된 설정:
App Config: SectionConfig (fromList [("app_name",SomeConfigValue (StringConfig "MyApp")),...])
App name: MyApp
Version: 1
```

## 📖 학습 순서

1. **BasicGADT.hs**: GADT의 기본 개념 이해
2. **DSLGADT.hs**: DSL에서의 GADT 활용법 학습
3. **AdvancedGADT.hs**: 고급 GADT와 타입 레벨 프로그래밍
4. **ConfigGADT.hs**: 실제 프로젝트에서의 GADT 활용

## 🔗 참고 자료

- [Haskell GADT 문서](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/gadt.html)
- [GADT 튜토리얼](https://en.wikibooks.org/wiki/Haskell/GADT)
- [타입 레벨 프로그래밍](https://wiki.haskell.org/Type_level_programming)

---

*이 예제들은 GADT의 다양한 활용법을 보여주며, 실제 프로젝트에서 타입 안전한 DSL을 구현하는 데 도움이 됩니다.*
