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

## ✅ 구현 완료된 기능

### 1단계: 기본 타입 및 AST 정의 ✅
- [x] Cap'n Proto 기본 타입 정의 (Void, Bool, Int8, Int16, Int32, Int64, UInt8, UInt16, UInt32, UInt64, Float32, Float64, Text, Data)
- [x] Struct, List, Union, Interface, Enum 등 AST 노드 정의
- [x] 간단한 AST 구조 (SimpleTypes.hs)
- [x] ID 관리 시스템

### 2단계: IDL 파서 구현 ✅
- [x] Megaparsec를 사용한 .capnp 문법 파서 (SimpleParser.hs)
- [x] @0x, @0x123 같은 ID 어노테이션 파싱
- [x] Struct 정의 파싱 (필드, 그룹, Union)
- [x] Interface 정의 파싱 (메서드, 파라미터)
- [x] Enum 정의 파싱
- [x] Const 정의 파싱
- [x] List(Type) 문법 파싱

### 3단계: 다중 언어 코드 생성기 구현 ✅
- [x] Haskell 코드 생성 (Record 타입, Type class, Enum, Const)
- [x] C++ 코드 생성 (Struct, Class, Enum, Namespace, Interface)
- [x] C# 코드 생성 (Class, Interface, Enum, Properties)
- [x] Python 코드 생성 (Dataclass, Class, Type hints)

### 4단계: 고급 코드 생성 기능 ✅
- [x] **예약어 검사**: 각 언어별 예약어 자동 변환
  - Haskell: `type` → `typeField`
  - C#: `type` → `Type` (PascalCase)
  - Python: `type` → `typeField`
- [x] **C++ 전방 선언**: 의존성 순서로 타입 선언 (enum → struct → interface)
- [x] **C# PascalCase**: 필드명 자동 변환 (`fieldName` → `FieldName`)
- [x] **문법 오류 방지**: 
  - 마지막 enum 값 콤마 제거
  - Record 필드 콤마 관리
  - C++ struct 의존성 순서 정렬

### 5단계: CLI 도구 구현 ✅
- [x] optparse-applicative를 사용한 CLI 인터페이스
- [x] 다중 언어 출력 지원 (`-l haskell|cpp|csharp|python`)
- [x] 출력 파일 지정 (`-o output.hs`)
- [x] 상세 출력 모드 (`-v`)

## 📋 구현된 기능 상세 설명

### 예약어 검사 시스템
각 언어별로 예약어를 자동으로 감지하고 안전한 이름으로 변환합니다:

**Haskell 예약어**: `type`, `class`, `data`, `where`, `if`, `then`, `else`, `case`, `of`, `let`, `in`, `do`, `import`, `module`, `instance`, `deriving`, `newtype`, `typeclass`, `forall`, `exists`

**C++ 예약어**: `class`, `struct`, `enum`, `union`, `namespace`, `public`, `private`, `protected`, `virtual`, `static`, `const`, `volatile`, `mutable`, `explicit`, `inline`, `friend`, `operator`, `template`, `typename`, `auto`, `decltype`, `nullptr`, `if`, `else`, `for`, `while`, `do`, `switch`, `case`, `default`, `break`, `continue`, `return`, `goto`, `try`, `catch`, `throw`, `new`, `delete`, `this`, `sizeof`, `typedef`, `extern`, `register`, `signed`, `unsigned`, `short`, `long`, `int`, `char`, `float`, `double`, `void`, `bool`, `true`, `false`

**C# 예약어**: `class`, `struct`, `enum`, `interface`, `namespace`, `public`, `private`, `protected`, `internal`, `virtual`, `static`, `const`, `readonly`, `volatile`, `mutable`, `explicit`, `implicit`, `inline`, `sealed`, `abstract`, `override`, `new`, `virtual`, `operator`, `event`, `delegate`, `using`, `if`, `else`, `for`, `while`, `do`, `switch`, `case`, `default`, `break`, `continue`, `return`, `goto`, `try`, `catch`, `throw`, `finally`, `lock`, `checked`, `unchecked`, `unsafe`, `fixed`, `stackalloc`, `sizeof`, `typeof`, `is`, `as`, `this`, `base`, `null`, `true`, `false`, `void`, `bool`, `byte`, `sbyte`, `char`, `decimal`, `double`, `float`, `int`, `uint`, `long`, `ulong`, `object`, `short`, `ushort`, `string`, `var`, `dynamic`, `ref`, `out`, `params`, `in`, `where`, `select`, `from`, `group`, `orderby`, `join`, `let`, `into`, `on`, `equals`, `by`, `ascending`, `descending`

**Python 예약어**: `and`, `as`, `assert`, `break`, `class`, `continue`, `def`, `del`, `elif`, `else`, `except`, `exec`, `finally`, `for`, `from`, `global`, `if`, `import`, `in`, `is`, `lambda`, `not`, `or`, `pass`, `print`, `raise`, `return`, `try`, `while`, `with`, `yield`, `True`, `False`, `None`, `type`

### C++ 전방 선언 시스템
C++ 컴파일 오류를 방지하기 위해 타입 의존성을 분석하고 올바른 순서로 선언합니다:

1. **Enum 선언**: 다른 타입에 의존하지 않으므로 먼저 선언
2. **Struct 선언**: 의존성 순서로 정렬하여 선언
3. **Interface 선언**: 모든 타입이 정의된 후 선언

### C# PascalCase 변환
C# 네이밍 컨벤션에 맞게 필드명을 자동으로 변환합니다:
- `fieldName` → `FieldName`
- `phoneNumber` → `PhoneNumber`
- `lastSeen` → `LastSeen`

### 문법 오류 방지
각 언어의 문법 규칙에 맞게 코드를 생성합니다:

**Haskell**:
- Record 타입의 마지막 필드에 콤마 제거
- Type class 메서드명 소문자 변환

**C++**:
- Enum의 마지막 값에 콤마 제거
- Struct 의존성 순서 정렬

**C#**:
- Enum의 마지막 값에 콤마 제거
- PascalCase 필드명 변환

**Python**:
- Dataclass 필드 정의
- Type hint 사용

## 🔧 향후 구현 계획

### 6단계: 스키마 검증 및 최적화
- [ ] 스키마 유효성 검증
- [ ] ID 충돌 검사
- [ ] 순환 참조 감지
- [ ] 메모리 레이아웃 최적화
- [ ] 스키마 진화 규칙 검증

### 7단계: Cap'n Proto 직렬화
- [ ] Wire format 구현 (Little-endian, 8-byte aligned)
- [ ] Segment 기반 메모리 관리
- [ ] Pointer 체인 관리
- [ ] Infinite speed 직렬화
- [ ] 메모리 정렬 최적화

### 8단계: RPC 프레임워크
- [ ] RPC 메시지 프레임워크
- [ ] 메서드 호출/응답 처리
- [ ] 스트리밍 RPC 지원
- [ ] 에러 처리 및 예외 전파
- [ ] 타임아웃 및 재시도 로직

### 9단계: 네트워크 통신
- [ ] TCP/UDP 소켓 통신
- [ ] 메시지 프레이밍
- [ ] 연결 관리
- [ ] 멀티플렉싱 지원
- [ ] SSL/TLS 지원 (선택사항)

### 10단계: 스키마 진화 지원
- [ ] Backward compatibility 검증
- [ ] Forward compatibility 검증
- [ ] 필드 추가/제거 처리
- [ ] 타입 변경 처리
- [ ] Interface 변경 처리

### 11단계: 테스트 및 최적화
- [ ] 단위 테스트 작성
- [ ] 통합 테스트 작성
- [ ] RPC 테스트
- [ ] 네트워크 테스트
- [ ] 성능 벤치마크
- [ ] 메모리 프로파일링

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
