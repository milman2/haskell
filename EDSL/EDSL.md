# EDSL (Embedded Domain Specific Language)

## EDSL이란?

EDSL(Embedded Domain Specific Language)은 **호스트 언어에 내장된 도메인 특화 언어**입니다. 기존 프로그래밍 언어의 문법과 타입 시스템을 활용하여 특정 도메인에 특화된 언어를 만드는 기법입니다.

## EDSL의 특징

### 1. **호스트 언어에 내장**
- 새로운 파서나 컴파일러를 만들 필요 없음
- 기존 언어의 타입 시스템과 문법 활용
- IDE 지원, 디버깅 도구 등 기존 도구 활용 가능

### 2. **도메인 특화**
- 특정 문제 영역에 최적화된 문법
- 도메인 전문가가 이해하기 쉬운 표현
- 복잡한 로직을 간단하게 표현

### 3. **타입 안전성**
- 컴파일 타임에 오류 검출
- 잘못된 사용을 방지
- 리팩토링 시 안전성 보장

## EDSL 예제들

### 1. **수학 표현식 EDSL (SimpleMath.hs)**

```haskell
-- 수학 표현식을 Haskell 코드로 작성
example1 :: MathExpr
example1 = (var "x" + 2) * (var "y" - 1)

example2 :: MathExpr
example2 = sin (var "theta") + cos (var "theta")

-- 변수 환경에서 계산
let env = [("x", 3.0), ("y", 4.0), ("theta", pi/4)]
eval env example1  -- 15.0
```

**장점:**
- 수학 표현식을 자연스럽게 작성
- 변수 바인딩과 계산 분리
- 타입 안전한 수학 연산

### 2. **데이터베이스 쿼리 EDSL (QueryDSL.hs)**

```haskell
-- SQL과 유사한 쿼리를 Haskell로 작성
example1 :: Query
example1 = select [Column "name" "VARCHAR", Column "age" "INT"] users
    `where_` (Column "age" "INT" >. VInt 18)
    `orderBy_` [Column "name" "VARCHAR"]

-- SQL로 변환
toSQL example1  -- "SELECT name, age FROM users WHERE age > 18 ORDER BY name"
```

**장점:**
- SQL 인젝션 공격 방지
- 컴파일 타임 쿼리 검증
- 복잡한 쿼리 조합 가능

### 3. **설정 파일 EDSL (ConfigDSL.hs)**

```haskell
-- 설정을 Haskell 코드로 작성
appConfig :: ConfigBuilder ()
appConfig = do
    set "app_name" (string "My Application")
    set "port" (int 8080)
    set "database" (map_ [
        ("host", string "localhost"),
        ("port", int 5432)
    ])

-- JSON으로 변환
configToJSON config  -- {"app_name": "My Application", "port": 8080, ...}
```

**장점:**
- 설정 파일의 문법 오류 방지
- 프로그래밍 언어의 모든 기능 활용
- 동적 설정 생성 가능

## EDSL vs PDSL (Pure Domain Specific Language)

### EDSL (Embedded Domain Specific Language)

#### **장점:**
1. **개발 비용 낮음**
   - 새로운 파서/컴파일러 불필요
   - 기존 도구와 생태계 활용

2. **타입 안전성**
   - 호스트 언어의 타입 시스템 활용
   - 컴파일 타임 오류 검출

3. **IDE 지원**
   - 자동완성, 문법 하이라이팅
   - 리팩토링 도구 지원

4. **디버깅 용이**
   - 호스트 언어의 디버거 활용
   - 스택 트레이스와 오류 메시지

5. **확장성**
   - 호스트 언어의 모든 기능 활용
   - 라이브러리와 프레임워크 통합

#### **단점:**
1. **문법 제약**
   - 호스트 언어의 문법에 제한됨
   - 도메인 특화 문법 구현 어려움

2. **성능 오버헤드**
   - 호스트 언어의 런타임 오버헤드
   - 최적화 제한

3. **학습 곡선**
   - 호스트 언어 지식 필요
   - 도메인 전문가가 접근하기 어려울 수 있음

### PDSL (Pure Domain Specific Language)

#### **장점:**
1. **완전한 자유도**
   - 도메인에 최적화된 문법 설계
   - 자연스러운 표현력

2. **성능 최적화**
   - 도메인 특화 컴파일러
   - 최적화된 코드 생성

3. **도메인 전문가 친화적**
   - 비개발자도 쉽게 이해
   - 자연어에 가까운 문법

4. **독립성**
   - 호스트 언어에 의존하지 않음
   - 자체 생태계 구축 가능

#### **단점:**
1. **높은 개발 비용**
   - 파서, 컴파일러, 런타임 개발
   - 도구와 생태계 구축 필요

2. **유지보수 부담**
   - 별도의 언어 엔진 유지보수
   - 버전 관리와 호환성 문제

3. **제한된 기능**
   - 도메인 외 기능 구현 어려움
   - 다른 시스템과의 통합 복잡

4. **학습 비용**
   - 새로운 언어 학습 필요
   - 도구와 환경 설정

## 실제 사용 사례

### EDSL 사용 사례

#### **1. Parsec (파싱 라이브러리)**
```haskell
-- 파서 조합자로 문법 정의
expr :: Parser Expr
expr = do
    e1 <- term
    op <- operator
    e2 <- expr
    return (BinOp op e1 e2)
```

#### **2. Lens (데이터 접근 라이브러리)**
```haskell
-- 데이터 구조 접근을 함수형으로
user ^. name  -- 사용자 이름 접근
user & name .~ "John"  -- 이름 변경
```

#### **3. Yesod (웹 프레임워크)**
```haskell
-- HTML을 Haskell로 작성
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Home"
    [whamlet|<h1>Welcome to my site!|]
```

### PDSL 사용 사례

#### **1. SQL**
```sql
-- 데이터베이스 쿼리 전용 언어
SELECT name, age FROM users WHERE age > 18 ORDER BY name;
```

#### **2. CSS**
```css
/* 스타일링 전용 언어 */
.button {
    background-color: blue;
    color: white;
    padding: 10px;
}
```

#### **3. HTML**
```html
<!-- 마크업 전용 언어 -->
<div class="container">
    <h1>Title</h1>
    <p>Content</p>
</div>
```

## EDSL 구현 기법

### 1. **AST (Abstract Syntax Tree) 기법**
```haskell
-- 표현식을 데이터 타입으로 정의
data Expr = Add Expr Expr | Mul Expr Expr | Lit Int

-- 연산자 오버로딩으로 자연스러운 문법
instance Num Expr where
    (+) = Add
    (*) = Mul
    fromInteger = Lit . fromInteger
```

### 2. **모나드 기법**
```haskell
-- 상태를 가진 계산을 모나드로
newtype State s a = State (s -> (a, s))

-- do 표기법으로 자연스러운 문법
config = do
    set "name" "value"
    set "port" 8080
```

### 3. **타입 클래스 기법**
```haskell
-- 타입 클래스로 인터페이스 정의
class Monad m => MonadState s m where
    get :: m s
    put :: s -> m ()

-- 다양한 구현체 제공
instance MonadState s (State s) where
    get = State $ \s -> (s, s)
    put s = State $ \_ -> ((), s)
```

## EDSL 설계 원칙

### 1. **단순성**
- 도메인에 필요한 최소한의 기능만 제공
- 복잡한 기능은 조합으로 구현

### 2. **일관성**
- 일관된 네이밍과 문법 사용
- 예측 가능한 동작

### 3. **확장성**
- 새로운 기능 추가가 쉬운 구조
- 모듈화된 설계

### 4. **타입 안전성**
- 잘못된 사용을 컴파일 타임에 방지
- 명확한 타입 시그니처

## 요약

EDSL은:

**장점:**
- 개발 비용 낮음
- 타입 안전성
- IDE 지원
- 디버깅 용이
- 확장성

**단점:**
- 문법 제약
- 성능 오버헤드
- 학습 곡선

**PDSL과의 차이:**
- EDSL: 호스트 언어에 내장, 개발 비용 낮음, 문법 제약
- PDSL: 독립적 언어, 완전한 자유도, 높은 개발 비용

EDSL은 **도메인 특화 언어의 장점과 호스트 언어의 생태계를 모두 활용**할 수 있는 강력한 기법입니다!

## 예제 실행 방법

### 1. **GHC로 직접 컴파일 (간단한 방법)**

```bash
# EDSL 디렉토리로 이동
cd /home/milman2/haskell/EDSL

# 컴파일
ghc --make Main.hs -o edslexample

# 실행
./edslexample
```

### 2. **Stack 사용 (권장 방법)**

```bash
# EDSL 디렉토리로 이동
cd /home/milman2/haskell/EDSL

# Stack 프로젝트 초기화
stack init

# 프로젝트 빌드
stack build

# 실행
stack exec EDSL
```

### 3. **GHCi에서 개별 모듈 테스트**

```bash
# GHCi 실행
ghci

# 모듈 로드
:load SimpleMath.hs
:load QueryDSL.hs
:load ConfigDSL.hs

# 개별 함수 테스트
testMath
testQuery
testConfig
```

### 4. **실행 결과 예시**

```
=== Simple Math EDSL Examples ===

Example 1: (x + 2) * (y - 1)
Expression: ((x + 2.0) * (y - 1.0))
Result: 15.0

Example 2: sin(theta) + cos(theta)
Expression: (sin(theta) + cos(theta))
Result: 1.414213562373095

Example 3: log(x * y) + z
Expression: (log((x * y)) + z)
Result: 4.484906649788

Environment:
  x = 3.0
  y = 4.0
  theta = 0.7853981633974483
  z = 2.0
===================================================

=== Query DSL Examples ===

Example 1: Select name, age from users where age > 18 order by name
SQL: SELECT name, age FROM users WHERE age > 18 ORDER BY name

Example 2: Select name, price from products where price < 100.0 and category = 'electronics' limit 10
SQL: SELECT name, price FROM products WHERE (price < 100.0 AND category = 'electronics') LIMIT 10

Example 3: Select id, name, email from users where age > 25 or name = 'admin'
SQL: SELECT id, name, email FROM users WHERE (age > 25 OR name = 'admin')

===================================================

=== Config DSL Examples ===

Configuration as JSON:
{"app_name": "My Application", "database": {"host": "localhost", "name": "myapp", "port": 5432, "ssl": true}, "debug": true, "features": ["auth", "logging", "metrics"], "logging": {"file": "/var/log/app.log", "level": "info", "max_size": 100}, "port": 8080, "timeout": 30.0, "version": "1.0.0"}

Read configuration:
Configuration incomplete

Individual values:
App name: My Application
Port: 8080
Debug: True
```

### 5. **문제 해결**

#### **컴파일 오류가 발생하는 경우:**
```bash
# 필요한 패키지 설치
cabal install --lib containers

# 또는 Stack 사용
stack install containers
```

#### **문법 오류가 발생하는 경우:**
- 들여쓰기 확인 (Haskell은 들여쓰기에 민감)
- 괄호와 대괄호 매칭 확인
- 세미콜론과 쉼표 사용 확인

#### **타입 오류가 발생하는 경우:**
- 함수 시그니처 확인
- 타입 클래스 인스턴스 정의 확인
- import 문 확인

### 6. **개발 환경 설정**

#### **VS Code/Cursor 설정:**
```json
{
    "folders": [
        {
            "name": "EDSL Examples",
            "path": "./EDSL"
        }
    ],
    "settings": {
        "haskell.serverExecutablePath": "haskell-language-server-wrapper",
        "haskell.formattingProvider": "ormolu",
        "haskell.hlint.enable": true
    },
    "extensions": {
        "recommendations": [
            "haskell.haskell",
            "justusadam.language-haskell"
        ]
    }
}
```

#### **필요한 도구:**
- GHC (Glasgow Haskell Compiler)
- Stack (Haskell 프로젝트 관리 도구)
- Cabal (Haskell 패키지 관리 도구)

## 참고 자료

- [Haskell EDSL 위키](https://wiki.haskell.org/Embedded_domain_specific_language)
- [Parsec 파싱 라이브러리](https://hackage.haskell.org/package/parsec)
- [Lens 데이터 접근 라이브러리](https://hackage.haskell.org/package/lens)
- [Yesod 웹 프레임워크](https://www.yesodweb.com/)
- [GHC 사용자 가이드](https://downloads.haskell.org/ghc/latest/docs/users_guide/)
- [Stack 문서](https://docs.haskellstack.org/)
