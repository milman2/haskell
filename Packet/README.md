# Packet Generator DSL 프로젝트

이 프로젝트는 Haskell DSL을 사용하여 다양한 패킷 생성기를 구현합니다. Protobuf, FlatBuffers, Cap'n Proto 스타일의 IDL(Interface Definition Language)을 파싱하고, 타입 안전한 직렬화/역직렬화 코드를 생성합니다.

## 📁 프로젝트 구조

```
Packet/
├── Protobuf/           # Protocol Buffers DSL 구현
│   ├── src/            # 소스 코드
│   ├── examples/       # 예제 .proto 파일들
│   ├── tests/          # 테스트 코드
│   ├── Protobuf.cabal  # Cabal 설정
│   └── TODO.md         # 구현 계획
├── FlatBuffers/        # FlatBuffers DSL 구현
│   ├── src/            # 소스 코드
│   ├── examples/       # 예제 .fbs 파일들
│   ├── tests/          # 테스트 코드
│   ├── benchmarks/     # 성능 벤치마크
│   ├── FlatBuffers.cabal # Cabal 설정
│   └── TODO.md         # 구현 계획
├── CapnProto/          # Cap'n Proto DSL 구현
│   ├── src/            # 소스 코드
│   ├── examples/       # 예제 .capnp 파일들
│   ├── tests/          # 테스트 코드
│   ├── benchmarks/     # 성능 벤치마크
│   ├── CapnProto.cabal # Cabal 설정
│   └── TODO.md         # 구현 계획
└── README.md           # 이 파일
```

## 🎯 각 프로젝트의 특징

### 1. Protobuf DSL
- **목표**: Protocol Buffers 스타일의 패킷 생성기
- **특징**: 
  - `.proto` 파일 파싱
  - 타입 안전한 직렬화/역직렬화
  - RPC 서비스 지원
  - 스키마 진화 지원

### 2. FlatBuffers DSL
- **목표**: FlatBuffers 스타일의 패킷 생성기
- **특징**:
  - `.fbs` 파일 파싱
  - Zero-copy 직렬화
  - 스키마 진화 지원
  - 메모리 효율적인 바이너리 포맷

### 3. Cap'n Proto DSL
- **목표**: Cap'n Proto 스타일의 패킷 생성기
- **특징**:
  - `.capnp` 파일 파싱
  - Infinite speed 직렬화
  - 네이티브 RPC 지원
  - 네트워크 통신 최적화

## 🔧 공통 기술 스택

### 핵심 라이브러리
- **파싱**: `megaparsec` - 강력한 파서 컴비네이터
- **코드 생성**: `template-haskell` - 컴파일 타임 코드 생성
- **바이너리**: `bytestring`, `binary`, `cereal` - 효율적인 바이너리 처리
- **CLI**: `optparse-applicative` - 사용자 친화적인 CLI 인터페이스

### 고급 기능
- **메모리 관리**: `primitive`, `vector` - 저수준 메모리 제어
- **네트워크**: `network`, `async`, `stm` - 고성능 네트워크 통신
- **테스트**: `hspec`, `quickcheck` - 포괄적인 테스트 프레임워크
- **벤치마크**: `criterion` - 정확한 성능 측정

## 🚀 구현 계획

### Phase 1: 기본 구조 설정
1. 각 프로젝트의 기본 Cabal 설정
2. 예제 IDL 파일 작성
3. 프로젝트 구조 설계

### Phase 2: 파서 구현
1. Megaparsec를 사용한 IDL 파서
2. AST 정의 및 검증
3. 파서 테스트 작성

### Phase 3: 코드 생성기 구현
1. Template Haskell을 사용한 코드 생성
2. 타입 안전한 직렬화/역직렬화 함수 생성
3. 코드 생성 테스트 작성

### Phase 4: 직렬화 엔진 구현
1. 각 포맷의 Wire format 구현
2. 성능 최적화
3. 메모리 효율성 개선

### Phase 5: 고급 기능 구현
1. RPC 프레임워크 (Protobuf, Cap'n Proto)
2. 스키마 진화 지원
3. 네트워크 통신 (Cap'n Proto)

### Phase 6: CLI 도구 및 테스트
1. 사용자 친화적인 CLI 인터페이스
2. 포괄적인 테스트 스위트
3. 성능 벤치마크

## 📚 학습 목표

### DSL 설계
- GADT를 사용한 타입 안전한 AST
- Free Monad를 사용한 DSL 인터프리터
- EDSL과 PDSL의 차이점 이해

### 파싱 기술
- Megaparsec를 사용한 복잡한 문법 파싱
- 파서 컴비네이터의 조합
- 에러 처리 및 복구

### 코드 생성
- Template Haskell을 사용한 메타프로그래밍
- 컴파일 타임 코드 생성
- 타입 안전성 보장

### 바이너리 처리
- 효율적인 직렬화 알고리즘
- 메모리 레이아웃 최적화
- Zero-copy 기술

### 네트워크 프로그래밍
- 고성능 네트워크 통신
- RPC 프레임워크 설계
- 동시성 및 병렬성

## 🎯 예상 결과물

각 프로젝트는 다음과 같은 기능을 제공할 예정입니다:

```bash
# Protobuf Generator
./protobuf-generator --input person.proto --output Person.hs

# FlatBuffers Generator  
./flatbuffers-generator --input monster.fbs --output Monster.hs

# Cap'n Proto Generator
./capnproto-generator --input addressbook.capnp --output AddressBook.hs
```

생성된 코드는 타입 안전하고 고성능인 직렬화/역직렬화 함수를 포함합니다.

## 🔗 참고 자료

- [Protocol Buffers Documentation](https://developers.google.com/protocol-buffers/)
- [FlatBuffers Documentation](https://google.github.io/flatbuffers/)
- [Cap'n Proto Documentation](https://capnproto.org/)
- [Megaparsec Tutorial](https://markkarpov.com/tutorial/megaparsec.html)
- [Template Haskell Documentation](https://hackage.haskell.org/package/template-haskell)

---

*이 프로젝트는 Haskell의 강력한 타입 시스템과 DSL 기능을 활용하여 현대적인 패킷 생성기를 구현하는 것을 목표로 합니다.*
