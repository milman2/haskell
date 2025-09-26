# Cap'n Proto Packet Generator DSL 프로젝트

## 📋 프로젝트 개요
Haskell DSL을 사용하여 Cap'n Proto 스타일의 패킷 생성기를 구현합니다. Infinite speed 직렬화와 RPC 지원을 포함한 고성능 통신 프로토콜을 생성합니다.

## 🎯 목표
- `.capnp` 파일을 파싱하는 DSL 구현
- Infinite speed 직렬화 (직렬화/역직렬화 없이 메모리 직접 접근)
- RPC(Remote Procedure Call) 지원
- 스키마 진화 및 버전 관리
- 고성능 네트워크 통신 지원

## 📁 프로젝트 구조
```
CapnProto/
├── src/
│   ├── CapnProto/
│   │   ├── Parser.hs          # .capnp 파일 파서 (Megaparsec)
│   │   ├── AST.hs             # Cap'n Proto AST 정의
│   │   ├── Schema.hs          # 스키마 정의 및 검증
│   │   ├── CodeGen.hs         # Haskell 코드 생성기
│   │   ├── Serialize.hs       # Cap'n Proto 직렬화
│   │   ├── RPC.hs             # RPC 프레임워크
│   │   ├── MemoryLayout.hs    # 메모리 레이아웃 관리
│   │   ├── Network.hs         # 네트워크 통신
│   │   └── Types.hs           # 기본 타입 정의
│   └── Main.hs                # CLI 메인 프로그램
├── examples/
│   ├── addressbook.capnp      # 예제 .capnp 파일
│   ├── calculator.capnp       # RPC 예제
│   ├── schema_evolution.capnp # 스키마 진화 예제
│   └── generated/             # 생성된 Haskell 코드
├── tests/
│   ├── ParserTest.hs          # 파서 테스트
│   ├── SchemaTest.hs          # 스키마 검증 테스트
│   ├── SerializeTest.hs       # 직렬화 테스트
│   ├── RPCTest.hs             # RPC 테스트
│   └── NetworkTest.hs         # 네트워크 테스트
├── benchmarks/
│   ├── SerializeBench.hs      # 직렬화 벤치마크
│   ├── RPCBench.hs            # RPC 성능 벤치마크
│   └── NetworkBench.hs        # 네트워크 성능 벤치마크
├── CapnProto.cabal            # Cabal 설정
└── TODO.md                    # 이 파일
```

## 🔧 구현 단계

### 1단계: 기본 타입 및 AST 정의
- [ ] Cap'n Proto 기본 타입 정의 (Void, Bool, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Float32, Float64, Text, Data)
- [ ] Struct, List, Union, Interface, Enum 등 AST 노드 정의
- [ ] GADT를 사용한 타입 안전한 AST 구조
- [ ] ID 관리 시스템

### 2단계: IDL 파서 구현
- [ ] Megaparsec를 사용한 .capnp 문법 파서
- [ ] @0x, @0x123 같은 ID 어노테이션 파싱
- [ ] Struct 정의 파싱 (필드, 그룹, Union)
- [ ] Interface 정의 파싱 (메서드, 파라미터)
- [ ] Enum 정의 파싱
- [ ] Const 정의 파싱
- [ ] Annotation 정의 파싱
- [ ] Using 문 파싱

### 3단계: 스키마 검증 및 최적화
- [ ] 스키마 유효성 검증
- [ ] ID 충돌 검사
- [ ] 순환 참조 감지
- [ ] 메모리 레이아웃 최적화
- [ ] 스키마 진화 규칙 검증

### 4단계: 코드 생성기 구현
- [ ] AST를 Haskell 타입으로 변환
- [ ] Template Haskell을 사용한 코드 생성
- [ ] Struct 타입 생성
- [ ] Interface 타입 생성
- [ ] RPC 클라이언트/서버 코드 생성
- [ ] Const 값 생성

### 5단계: Cap'n Proto 직렬화
- [ ] Wire format 구현 (Little-endian, 8-byte aligned)
- [ ] Segment 기반 메모리 관리
- [ ] Pointer 체인 관리
- [ ] Infinite speed 직렬화
- [ ] 메모리 정렬 최적화

### 6단계: RPC 프레임워크
- [ ] RPC 메시지 프레임워크
- [ ] 메서드 호출/응답 처리
- [ ] 스트리밍 RPC 지원
- [ ] 에러 처리 및 예외 전파
- [ ] 타임아웃 및 재시도 로직

### 7단계: 네트워크 통신
- [ ] TCP/UDP 소켓 통신
- [ ] 메시지 프레이밍
- [ ] 연결 관리
- [ ] 멀티플렉싱 지원
- [ ] SSL/TLS 지원 (선택사항)

### 8단계: 스키마 진화 지원
- [ ] Backward compatibility 검증
- [ ] Forward compatibility 검증
- [ ] 필드 추가/제거 처리
- [ ] 타입 변경 처리
- [ ] Interface 변경 처리

### 9단계: CLI 도구 구현
- [ ] optparse-applicative를 사용한 CLI 인터페이스
- [ ] 스키마 검증 모드
- [ ] 코드 생성 모드
- [ ] RPC 서버/클라이언트 모드
- [ ] 성능 벤치마크 모드

### 10단계: 테스트 및 최적화
- [ ] 단위 테스트 작성
- [ ] 통합 테스트 작성
- [ ] RPC 테스트
- [ ] 네트워크 테스트
- [ ] 성능 벤치마크
- [ ] 메모리 프로파일링

## 📚 사용할 라이브러리
- **파싱**: `megaparsec`, `text`
- **코드 생성**: `template-haskell`, `prettyprinter`
- **바이너리**: `bytestring`, `binary`, `cereal`
- **메모리**: `primitive`, `vector`
- **네트워크**: `network`, `async`, `stm`
- **CLI**: `optparse-applicative`
- **테스트**: `hspec`, `quickcheck`
- **벤치마크**: `criterion`

## 🎯 예상 결과물
```haskell
-- addressbook.capnp에서 생성된 코드
data Person = Person
    { personId :: UInt32
    , personName :: Text
    , personEmail :: Text
    , personPhones :: List PhoneNumber
    , personLastSeen :: Maybe Date
    } deriving (Show, Eq)

data PhoneNumber = PhoneNumber
    { phoneNumber :: Text
    , phoneType :: PhoneType
    } deriving (Show, Eq)

-- RPC Interface
class AddressBookService m where
    addPerson :: Person -> m UInt32
    getPerson :: UInt32 -> m (Maybe Person)
    listPeople :: m (List Person)
```

## 🔍 Cap'n Proto 특징
- **Infinite Speed**: 직렬화/역직렬화 없이 메모리 직접 접근
- **RPC Support**: 네이티브 RPC 프레임워크
- **Schema Evolution**: 강력한 스키마 진화 지원
- **Cross-platform**: 다양한 언어 지원
- **Memory Efficient**: 최소한의 메모리 사용
- **Network Ready**: 네트워크 통신 최적화

## 🔍 참고 자료
- [Cap'n Proto Documentation](https://capnproto.org/)
- [Cap'n Proto Schema Language](https://capnproto.org/language.html)
- [Cap'n Proto RPC](https://capnproto.org/rpc.html)
- [Infinite Speed Serialization](https://capnproto.org/news/2013-05-15-capnproto-0.4-time-travel.html)
