# Megaparsec
- 정교한 에러 메시지
- MonadParsec 타입 클래스
- Text기반 입력

# 학습 포인트
- ParsecT와 모나트 스택 통합
- errorBundleProperty, withRecovery, observing, parsecError
- Text.Megaparsec.Char.Lexer를 통한 숫자, 공백 처리
- tokens, takeWhile, takeP

# Megaparsec 학습 로드맵
1️⃣ 기본 구조 이해하기
Parsec → ParsecT → MonadParsec 타입 클래스

Text 기반 입력 처리 (Data.Text)

errorBundlePretty로 에러 메시지 출력

2️⃣ 주요 컴비네이터 익히기
many, some, choice, try, sepBy, between

lexeme, symbol, space 등 Text.Megaparsec.Char.Lexer 활용

3️⃣ 에러 처리와 백트래킹
try로 실패 시 백트래킹

<?>로 사용자 정의 에러 메시지

withRecovery, observing으로 에러 복구

4️⃣ 구조화된 파싱
AST 생성

중첩 구조, 괄호, 연산자 우선순위 처리