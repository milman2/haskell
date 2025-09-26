# PDSL (Pure Domain Specific Language) with Haskell

## PDSL이란?

PDSL(Pure Domain Specific Language)은 **독립적인 도메인 특화 언어**입니다. 이 문서에서는 Haskell을 사용하여 PDSL을 구현하는 방법을 보여줍니다. Haskell의 강력한 타입 시스템과 함수형 프로그래밍을 활용하여 안전하고 효율적인 PDSL을 만들 수 있습니다.

## PDSL의 특징

### 1. **완전한 독립성**
- 자체적인 문법과 구문
- 독립적인 파서와 컴파일러
- 호스트 언어에 의존하지 않음

### 2. **도메인 최적화**
- 특정 도메인에 완전히 최적화된 문법
- 자연스럽고 직관적인 표현
- 도메인 전문가가 쉽게 이해할 수 있는 문법

### 3. **성능 최적화**
- 도메인 특화 컴파일러
- 최적화된 코드 생성
- 런타임 오버헤드 최소화

### 4. **자체 생태계**
- 독립적인 도구와 라이브러리
- 전용 IDE와 디버거
- 자체 표준과 규격

## Haskell로 구현한 PDSL 예제들

### 1. **수학 표현식 PDSL (HaskellPDSL.hs)**

#### **문법 정의 (Haskell 데이터 타입)**
```haskell
data PDSLExpr
    = Number Double
    | String Text
    | Variable Text
    | Add PDSLExpr PDSLExpr
    | Subtract PDSLExpr PDSLExpr
    | Multiply PDSLExpr PDSLExpr
    | Divide PDSLExpr PDSLExpr
    | Equal PDSLExpr PDSLExpr
    | LessThan PDSLExpr PDSLExpr
    | GreaterThan PDSLExpr PDSLExpr
    | And PDSLExpr PDSLExpr
    | Or PDSLExpr PDSLExpr
    | Not PDSLExpr
    | If PDSLExpr PDSLExpr PDSLExpr
    deriving (Show, Eq)
```

#### **파서 구현**
```haskell
parsePDSL :: Text -> Either Text PDSLExpr
parsePDSL input = case parseExpr (words (unpack input)) of
    Left err -> Left (pack err)
    Right (expr, []) -> Right expr
    Right (_, remaining) -> Left ("Unexpected tokens: " `mappend` pack (unwords remaining))
```

#### **실행기 구현**
```haskell
evalPDSL :: PDSLEnv -> PDSLExpr -> Either Text PDSLValue
evalPDSL env (Add left right) = do
    l <- evalPDSL env left
    r <- evalPDSL env right
    case (l, r) of
        (VNumber a, VNumber b) -> Right (VNumber (a + b))
        (VString a, VString b) -> Right (VString (a `mappend` b))
        _ -> Left "Type error in addition"
```

#### **사용 예시**
```haskell
-- Haskell에서 PDSL 실행
runPDSL "2 + 3 * 4"        -- Result: 14.0
runPDSL "10 > 5 && 3 < 7"  -- Result: true
runPDSL "if 5 > 3 then 10 else 20"  -- Result: 10.0
```

### 2. **설정 파일 PDSL (ConfigPDSL.hs)**

#### **문법 정의**
```haskell
data ConfigExpr
    = ConfigString Text
    | ConfigNumber Double
    | ConfigBoolean Bool
    | ConfigList [ConfigExpr]
    | ConfigSection Text [(Text, ConfigExpr)]
    | ConfigInclude Text
    | ConfigCondition ConfigExpr ConfigExpr ConfigExpr
    deriving (Show, Eq)
```

#### **설정 파일 예시**
```haskell
configExample :: Text
configExample = pack $ unlines
    [ "app_name = \"My Application\""
    , "version = \"1.0.0\""
    , "debug = true"
    , "port = 8080"
    , "database_host = \"localhost\""
    , "database_port = 5432"
    , "features = [\"auth\", \"logging\", \"metrics\"]"
    ]
```

#### **파싱 및 검증**
```haskell
parseConfig :: Text -> Either Text [ConfigExpr]
validateConfig :: ConfigEnv -> [ConfigExpr] -> Either Text ConfigEnv
```

### 3. **쿼리 PDSL (QueryPDSL.hs)**

#### **문법 정의**
```haskell
data QueryExpr
    = Select [Text] Text (Maybe WhereClause) (Maybe OrderClause) (Maybe LimitClause)
    | Insert Text [(Text, ValueExpr)]
    | Update Text [(Text, ValueExpr)] (Maybe WhereClause)
    | Delete Text (Maybe WhereClause)
    deriving (Show, Eq)

data WhereClause = WhereClause Condition deriving (Show, Eq)
data Condition
    = Equal Text ValueExpr
    | NotEqual Text ValueExpr
    | LessThan Text ValueExpr
    | GreaterThan Text ValueExpr
    | And Condition Condition
    | Or Condition Condition
    deriving (Show, Eq)
```

#### **쿼리 파싱**
```haskell
parseQuery :: Text -> Either Text QueryExpr
parseQuery input = case words (unpack input) of
    ("SELECT" : rest) -> parseSelect rest
    ("INSERT" : rest) -> parseInsert rest
    ("UPDATE" : rest) -> parseUpdate rest
    ("DELETE" : rest) -> parseDelete rest
    _ -> Left "Invalid query"
```

#### **사용 예시**
```haskell
-- SQL과 유사한 쿼리를 Haskell에서 파싱
parseQuery "SELECT * FROM users WHERE age > 25"
parseQuery "SELECT name, email FROM users WHERE age > 25 AND name = \"Jane\""
```

## PDSL vs EDSL 비교

### **PDSL (Pure Domain Specific Language)**

#### **장점:**
1. **완전한 자유도**
   - 도메인에 최적화된 문법 설계
   - 자연스러운 표현력
   - 제약 없는 문법 설계

2. **성능 최적화**
   - 도메인 특화 컴파일러
   - 최적화된 코드 생성
   - 런타임 오버헤드 없음

3. **도메인 전문가 친화적**
   - 비개발자도 쉽게 이해
   - 자연어에 가까운 문법
   - 직관적인 표현

4. **독립성**
   - 호스트 언어에 의존하지 않음
   - 자체 생태계 구축 가능
   - 표준화된 문법

#### **단점:**
1. **높은 개발 비용**
   - 파서, 컴파일러, 런타임 개발
   - 도구와 생태계 구축 필요
   - 유지보수 부담

2. **학습 비용**
   - 새로운 언어 학습 필요
   - 도구와 환경 설정
   - 문서화와 교육 필요

3. **제한된 기능**
   - 도메인 외 기능 구현 어려움
   - 다른 시스템과의 통합 복잡
   - 확장성 제한

### **EDSL (Embedded Domain Specific Language)**

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

#### **단점:**
1. **문법 제약**
   - 호스트 언어의 문법에 제한됨
   - 도메인 특화 문법 구현 어려움

2. **성능 오버헤드**
   - 호스트 언어의 런타임 오버헤드
   - 최적화 제한

## PDSL 구현 과정

### 1. **문법 설계**
```yacc
// Yacc/Bison 문법 정의
%token NUMBER PLUS MINUS TIMES DIVIDE
%left PLUS MINUS
%left TIMES DIVIDE

%%
expression:
    NUMBER { $$ = $1; }
    | expression PLUS expression { $$ = $1 + $3; }
    | expression MINUS expression { $$ = $1 - $3; }
    ;
%%
```

### 2. **렉서 작성**
```lex
%{
#include "parser.tab.h"
%}

%%
[0-9]+(\.[0-9]+)?    { yylval.number = atof(yytext); return NUMBER; }
"+"                  { return PLUS; }
"-"                  { return MINUS; }
[ \t\n]              { /* ignore whitespace */ }
%%
```

### 3. **컴파일러/인터프리터 구현**
```c
// AST 노드 정의
typedef struct ASTNode {
    int type;
    double value;
    struct ASTNode* left;
    struct ASTNode* right;
} ASTNode;

// 코드 생성
double evaluate(ASTNode* node) {
    switch (node->type) {
        case NUMBER: return node->value;
        case PLUS: return evaluate(node->left) + evaluate(node->right);
        case MINUS: return evaluate(node->left) - evaluate(node->right);
        default: return 0.0;
    }
}
```

## 실제 PDSL 사례

### 1. **SQL (데이터베이스)**
- **목적**: 데이터베이스 조작
- **특징**: 선언적 문법, 집합 연산
- **사용자**: 데이터베이스 관리자, 개발자

### 2. **CSS (스타일링)**
- **목적**: 웹 페이지 스타일링
- **특징**: 계단식 스타일, 선택자 기반
- **사용자**: 웹 디자이너, 프론트엔드 개발자

### 3. **HTML (마크업)**
- **목적**: 웹 페이지 구조 정의
- **특징**: 태그 기반, 계층적 구조
- **사용자**: 웹 개발자, 콘텐츠 작성자

### 4. **JSON (데이터 교환)**
- **목적**: 데이터 직렬화/역직렬화
- **특징**: 경량, 인간이 읽기 쉬움
- **사용자**: API 개발자, 시스템 통합자

### 5. **LaTeX (문서 작성)**
- **목적**: 고품질 문서 작성
- **특징**: 명령 기반, 수학 공식 지원
- **사용자**: 학자, 연구자, 출판사

## PDSL 개발 도구

### 1. **파서 생성기**
- **Yacc/Bison**: C/C++용 파서 생성기
- **ANTLR**: 다중 언어 지원 파서 생성기
- **PEG**: 파싱 표현 문법

### 2. **렉서 생성기**
- **Flex/Lex**: C용 렉서 생성기
- **JFlex**: Java용 렉서 생성기
- **정규 표현식**: 간단한 토큰화

### 3. **컴파일러 도구**
- **LLVM**: 중간 표현과 코드 생성
- **GCC**: C 컴파일러 백엔드
- **자체 구현**: 인터프리터 또는 컴파일러

## PDSL 설계 원칙

### 1. **단순성**
- 최소한의 문법 요소
- 직관적인 키워드
- 일관된 문법 규칙

### 2. **표현력**
- 도메인 개념을 자연스럽게 표현
- 복잡한 작업을 간단하게 수행
- 추상화 수준 적절히 조절

### 3. **확장성**
- 새로운 기능 추가 용이
- 모듈화된 구조
- 플러그인 시스템

### 4. **성능**
- 효율적인 파싱
- 최적화된 실행
- 메모리 사용량 최소화

## Haskell PDSL 실행 방법

### 1. **GHC로 직접 컴파일**

```bash
# PDSL 디렉토리로 이동
cd /home/milman2/haskell/PDSL

# 컴파일
ghc --make Main.hs -o pdslexample

# 실행
./pdslexample
```

### 2. **Stack 사용 (권장)**

```bash
# PDSL 디렉토리로 이동
cd /home/milman2/haskell/PDSL

# 프로젝트 빌드
# stack init
stack build

# 실행
stack exec PDSL
```

### 3. **GHCi에서 개별 모듈 테스트**

```bash
# GHCi 실행
ghci

# 모듈 로드
:load HaskellPDSL.hs
:load ConfigPDSL.hs
:load QueryPDSL.hs

# 개별 함수 테스트
testPDSL
testConfigPDSL
testQueryPDSL
```

### 4. **실행 결과 예시**

```
=== Haskell PDSL Examples ===

Example 1: 2 + 3 * 4
Result: 14.0

Example 2: 10 > 5 && 3 < 7
Result: true

Example 3: if 5 > 3 then 10 else 20
Result: 10.0

Example 4: !(5 > 10)
Result: true

Example 5: (2 + 3) * (4 - 1)
Result: 15.0
===================================================

=== Config PDSL Examples ===

Configuration file:
app_name = "My Application"
version = "1.0.0"
debug = true
port = 8080
database_host = "localhost"
database_port = 5432
features = ["auth", "logging", "metrics"]

Parsed configuration:
ConfigSection "app_name" [("app_name",ConfigString "My Application")]
ConfigSection "version" [("version",ConfigString "1.0.0")]
ConfigSection "debug" [("debug",ConfigBoolean True)]
ConfigSection "port" [("port",ConfigNumber 8080.0)]
ConfigSection "database_host" [("database_host",ConfigString "localhost")]
ConfigSection "database_port" [("database_port",ConfigNumber 5432.0)]
ConfigSection "features" [("features",ConfigList [ConfigString "auth",ConfigString "logging",ConfigString "metrics"])]

Validated configuration:
Sections: ["app_name","version","debug","port","database_host","database_port","features"]

Individual values:
App name: My Application
Port: 8080.0
===================================================

=== Query PDSL Examples ===

Test database created with users table

Example 1: SELECT * FROM users
Found 3 rows
Row {rowData = fromList [("age",NumberValue 25.0),("email",StringValue "john@example.com"),("id",NumberValue 1.0),("name",StringValue "John")]}
Row {rowData = fromList [("age",NumberValue 30.0),("email",StringValue "jane@example.com"),("id",NumberValue 2.0),("name",StringValue "Jane")]}
Row {rowData = fromList [("age",NumberValue 35.0),("email",StringValue "bob@example.com"),("id",NumberValue 3.0),("name",StringValue "Bob")]}

Example 2: SELECT * FROM users WHERE age > 25
Found 2 rows
Row {rowData = fromList [("age",NumberValue 30.0),("email",StringValue "jane@example.com"),("id",NumberValue 2.0),("name",StringValue "Jane")]}
Row {rowData = fromList [("age",NumberValue 35.0),("email",StringValue "bob@example.com"),("id",NumberValue 3.0),("name",StringValue "Bob")]}

Example 3: SELECT * FROM users WHERE age > 25 AND name = "Jane"
Found 1 rows
Row {rowData = fromList [("age",NumberValue 30.0),("email",StringValue "jane@example.com"),("id",NumberValue 2.0),("name",StringValue "Jane")]}
```

## Haskell로 PDSL을 구현하는 장점

### 1. **타입 안전성**
- 컴파일 타임에 문법 오류 검출
- 잘못된 타입 사용 방지
- 리팩토링 시 안전성 보장

### 2. **함수형 프로그래밍**
- 순수 함수로 파서와 실행기 구현
- 부작용 없는 코드
- 테스트와 디버깅 용이

### 3. **패턴 매칭**
- AST 노드 처리에 최적화
- 명확하고 읽기 쉬운 코드
- 오류 처리 간소화

### 4. **모나드 활용**
- 파싱과 실행의 체계적 처리
- 오류 전파와 상태 관리
- 코드 조합성 향상

## 요약

Haskell로 구현한 PDSL은:

**특징:**
- Haskell의 강력한 타입 시스템 활용
- 함수형 프로그래밍으로 안전한 구현
- 패턴 매칭으로 명확한 문법 처리

**장점:**
- 타입 안전성
- 함수형 프로그래밍의 장점
- 패턴 매칭의 명확성
- 모나드의 체계적 처리

**EDSL과의 차이:**
- PDSL: 독립적 언어, 완전한 자유도, Haskell로 구현
- EDSL: 호스트 언어 내장, 문법 제약, Haskell 타입 시스템 활용

Haskell로 구현한 PDSL은 **타입 안전성과 함수형 프로그래밍의 장점을 모두 활용**하여 안전하고 효율적인 도메인 특화 언어를 만들 수 있는 강력한 접근법입니다!

## 참고 자료

- [Haskell 공식 문서](https://www.haskell.org/documentation/)
- [Haskell 타입 시스템](https://wiki.haskell.org/Type_system)
- [Haskell 모나드 튜토리얼](https://wiki.haskell.org/Monad_tutorials_timeline)
- [Parsec 파싱 라이브러리](https://hackage.haskell.org/package/parsec)
- [Attoparsec 고성능 파싱](https://hackage.haskell.org/package/attoparsec)
- [Megaparsec 현대적 파싱](https://hackage.haskell.org/package/megaparsec)
- [Haskell EDSL 위키](https://wiki.haskell.org/Embedded_domain_specific_language)
