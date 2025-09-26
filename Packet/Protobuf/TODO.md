# Protobuf Packet Generator DSL 프로젝트

## 📋 프로젝트 개요
Haskell DSL을 사용하여 Protocol Buffers 스타일의 패킷 생성기를 구현합니다. IDL(Interface Definition Language)을 파싱하고, 타입 안전한 직렬화/역직렬화 코드를 생성합니다.

## 🎯 목표
- `.proto` 파일을 파싱하는 DSL 구현
- AST 기반의 타입 안전한 데이터 구조 생성
- 바이너리 직렬화/역직렬화 코드 자동 생성
- CLI 도구로 사용 가능한 패킷 생성기

## 📁 프로젝트 구조
```
Protobuf/
├── src/
│   ├── Protobuf/
│   │   ├── Parser.hs          # .proto 파일 파서 (Megaparsec)
│   │   ├── AST.hs             # Protobuf AST 정의
│   │   ├── CodeGen.hs         # Haskell 코드 생성기
│   │   ├── Serialize.hs       # 바이너리 직렬화/역직렬화
│   │   └── Types.hs           # 기본 타입 정의
│   └── Main.hs                # CLI 메인 프로그램
├── examples/
│   ├── person.proto           # 예제 .proto 파일
│   └── generated/             # 생성된 Haskell 코드
├── tests/
│   ├── ParserTest.hs          # 파서 테스트
│   ├── CodeGenTest.hs         # 코드 생성 테스트
│   └── SerializeTest.hs       # 직렬화 테스트
├── Protobuf.cabal             # Cabal 설정
└── TODO.md                    # 이 파일
```

## 🔧 구현 단계

### 1단계: 기본 타입 및 AST 정의
- [ ] Protobuf 기본 타입 정의 (int32, string, bool, etc.)
- [ ] Message, Field, Enum 등 AST 노드 정의
- [ ] GADT를 사용한 타입 안전한 AST 구조

### 2단계: IDL 파서 구현
- [ ] Megaparsec를 사용한 .proto 문법 파서
- [ ] Message 정의 파싱
- [ ] Field 타입 및 번호 파싱
- [ ] Enum 정의 파싱
- [ ] Import/Export 문 파싱

### 3단계: 코드 생성기 구현
- [ ] AST를 Haskell 타입으로 변환
- [ ] Template Haskell을 사용한 코드 생성
- [ ] 직렬화/역직렬화 함수 생성
- [ ] Pretty printer를 사용한 코드 포맷팅

### 4단계: 바이너리 직렬화
- [ ] Wire format 구현 (Varint, Length-delimited, etc.)
- [ ] ByteString 기반 직렬화
- [ ] 역직렬화 및 검증
- [ ] 성능 최적화

### 5단계: CLI 도구 구현
- [ ] optparse-applicative를 사용한 CLI 인터페이스
- [ ] 파일 I/O 처리
- [ ] 에러 처리 및 사용자 피드백
- [ ] 배치 처리 지원

### 6단계: 테스트 및 문서화
- [ ] 단위 테스트 작성
- [ ] 통합 테스트 작성
- [ ] 성능 벤치마크
- [ ] 사용자 문서 작성

## 📚 사용할 라이브러리
- **파싱**: `megaparsec`, `text`
- **코드 생성**: `template-haskell`, `prettyprinter`
- **바이너리**: `bytestring`, `binary`, `cereal`
- **CLI**: `optparse-applicative`
- **테스트**: `hspec`, `quickcheck`

## 🎯 예상 결과물
```haskell
-- person.proto에서 생성된 코드
data Person = Person
    { personName :: Text
    , personAge :: Int32
    , personEmail :: Maybe Text
    } deriving (Show, Eq)

instance ProtobufMessage Person where
    encodeMessage = encodePerson
    decodeMessage = decodePerson
```

## 🔍 참고 자료
- [Protocol Buffers Language Guide](https://developers.google.com/protocol-buffers/docs/proto3)
- [Megaparsec Tutorial](https://markkarpov.com/tutorial/megaparsec.html)
- [Template Haskell Documentation](https://hackage.haskell.org/package/template-haskell)
