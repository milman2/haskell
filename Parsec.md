# Parsec
- many
- choice
- try
- sepBy

# 학습 포인트
- Parser a 타입의 의미
- do 블럭을 통한 파서 조합
- 에러 처리 방식

# 📁 프로젝트 구조
Parsec/
├── examples/                    # 13개의 단계별 학습 예제
│   ├── step1-basics/           # 1단계: 기본 개념 (3개 파일)
│   ├── step2-simple/           # 2단계: 간단한 파서 (4개 파일)
│   ├── step3-structured/       # 3단계: 구조화된 데이터 (3개 파일)
│   └── step4-error-handling/   # 4단계: 에러 처리 (3개 파일)
├── src/                        # 라이브러리 소스
├── app/                        # 실행 파일
├── test/                       # 테스트
├── package.yaml               # Stack 프로젝트 설정
└── README.md                  # 상세한 학습 가이드


```shell
cd /home/milman2/haskell/Parsec
stack build

# 1단계: 기본 개념
stack exec -- runhaskell examples/step1-basics/01_ParserType.hs

# 2단계: 간단한 파서
stack exec -- runhaskell examples/step2-simple/01_NumberParsing.hs

# 3단계: 구조화된 데이터
stack exec -- runhaskell examples/step3-structured/01_KeyValueParsing.hs

# 4단계: 에러 처리
stack exec -- runhaskell examples/step4-error-handling/01_Backtracking.hs
```