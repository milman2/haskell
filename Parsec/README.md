# Parsec 학습 가이드

Haskell의 Parsec 라이브러리를 단계별로 학습하는 프로젝트입니다.

## 📚 학습 로드맵

### 1️⃣ 기본 개념 익히기
- **Parser a 타입의 의미**
- **char, string, many, choice, try 등 기본 컴비네이터**
- **parse 함수로 입력을 파싱**

### 2️⃣ 간단한 파서 작성
- **숫자, 문자, 문자열 파싱**
- **리스트, 구분자(sepBy) 처리**
- **공백 무시 (spaces, lexeme)**

### 3️⃣ 구조화된 데이터 파싱
- **키-값 쌍 (key=value)**
- **괄호, 중첩 구조**
- **AST 생성**

### 4️⃣ 에러 처리와 디버깅
- **try로 백트래킹**
- **<?>로 사용자 정의 에러 메시지**
- **parseTest, parseFromFile로 테스트**

## 🚀 시작하기

### 필수 요구사항
- Stack (Haskell 빌드 도구)
- GHC (Glasgow Haskell Compiler)

### 설치 및 실행
```bash
# 프로젝트 클론
git clone <repository-url>
cd Parsec

# 의존성 설치
stack build

# 예제 실행
stack exec -- runhaskell examples/step1-basics/01_ParserType.hs
```

## 📁 프로젝트 구조

```
Parsec/
├── examples/                    # 학습 예제들
│   ├── step1-basics/           # 1단계: 기본 개념
│   │   ├── 01_ParserType.hs
│   │   ├── 02_BasicCombinators.hs
│   │   └── 03_ParseFunction.hs
│   ├── step2-simple/           # 2단계: 간단한 파서
│   │   ├── 01_NumberParsing.hs
│   │   ├── 02_StringParsing.hs
│   │   ├── 03_ListParsing.hs
│   │   └── 04_WhitespaceHandling.hs
│   ├── step3-structured/       # 3단계: 구조화된 데이터
│   │   ├── 01_KeyValueParsing.hs
│   │   ├── 02_BracketParsing.hs
│   │   └── 03_ASTGeneration.hs
│   └── step4-error-handling/   # 4단계: 에러 처리
│       ├── 01_Backtracking.hs
│       ├── 02_CustomErrorMessages.hs
│       └── 03_TestingAndDebugging.hs
├── src/                        # 라이브러리 소스
├── app/                        # 실행 파일
├── test/                       # 테스트
└── package.yaml               # Stack 프로젝트 설정
```

## 📖 학습 방법

### 1단계: 기본 개념 익히기
```bash
# Parser 타입 이해
stack exec -- runhaskell examples/step1-basics/01_ParserType.hs

# 기본 컴비네이터 학습
stack exec -- runhaskell examples/step1-basics/02_BasicCombinators.hs

# parse 함수 사용법
stack exec -- runhaskell examples/step1-basics/03_ParseFunction.hs
```

### 2단계: 간단한 파서 작성
```bash
# 숫자 파싱
stack exec -- runhaskell examples/step2-simple/01_NumberParsing.hs

# 문자열 파싱
stack exec -- runhaskell examples/step2-simple/02_StringParsing.hs

# 리스트 파싱
stack exec -- runhaskell examples/step2-simple/03_ListParsing.hs

# 공백 처리
stack exec -- runhaskell examples/step2-simple/04_WhitespaceHandling.hs
```

### 3단계: 구조화된 데이터 파싱
```bash
# 키-값 쌍 파싱
stack exec -- runhaskell examples/step3-structured/01_KeyValueParsing.hs

# 괄호 파싱
stack exec -- runhaskell examples/step3-structured/02_BracketParsing.hs

# AST 생성
stack exec -- runhaskell examples/step3-structured/03_ASTGeneration.hs
```

### 4단계: 에러 처리와 디버깅
```bash
# 백트래킹
stack exec -- runhaskell examples/step4-error-handling/01_Backtracking.hs

# 사용자 정의 에러 메시지
stack exec -- runhaskell examples/step4-error-handling/02_CustomErrorMessages.hs

# 테스트와 디버깅
stack exec -- runhaskell examples/step4-error-handling/03_TestingAndDebugging.hs
```

## 🔧 주요 개념

### Parser 타입
```haskell
type Parser a = Parsec String () a
```
- `Parser a`는 문자열을 파싱하여 `a` 타입의 값을 반환하는 파서입니다.

### 기본 컴비네이터
- `char 'a'`: 특정 문자 파싱
- `string "hello"`: 특정 문자열 파싱
- `many p`: 0개 이상 반복
- `many1 p`: 1개 이상 반복
- `p1 <|> p2`: 선택 (p1 또는 p2)
- `try p`: 백트래킹 허용

### 파싱 함수
- `parse p name input`: 파서 실행
- `parseTest p input`: 테스트용 파싱
- `parseFromFile p filename`: 파일에서 파싱

## 📝 예제 실행 결과

### 1단계 예제
```
=== Parser a 타입 이해하기 ===

1. char 'a' 파서:
Right 'a'
Left (line 1, column 1):
unexpected "b"
expecting "a"

2. string "hello" 파서:
Right "hello"
Left (line 1, column 1):
unexpected "w"
expecting "hello"
```

### 2단계 예제
```
=== 숫자 파싱 예제 ===

1. 양수 정수 파싱:
Right 123
Right 0
Left (line 1, column 1):
unexpected "-"
expecting digit
```

## 🎯 학습 목표

각 단계를 완료하면 다음을 할 수 있게 됩니다:

1. **1단계**: Parsec의 기본 개념을 이해하고 간단한 파서를 작성할 수 있습니다.
2. **2단계**: 숫자, 문자열, 리스트 등을 파싱할 수 있습니다.
3. **3단계**: 복잡한 구조화된 데이터를 파싱하고 AST를 생성할 수 있습니다.
4. **4단계**: 에러를 효과적으로 처리하고 파서를 디버깅할 수 있습니다.

## 🤝 기여하기

이 프로젝트는 학습 목적으로 만들어졌습니다. 개선 사항이나 추가 예제가 있다면 기여해주세요!

## 📄 라이선스

BSD-3-Clause License

## 🔗 참고 자료

- [Parsec 공식 문서](https://hackage.haskell.org/package/parsec)
- [Real World Haskell - Parsec](http://book.realworldhaskell.org/read/using-parsec.html)
- [Haskell Wiki - Parsec](https://wiki.haskell.org/Parsec)