# FlatBuffers Packet Generator DSL 프로젝트

## 📋 프로젝트 개요
Haskell DSL을 사용하여 FlatBuffers 스타일의 패킷 생성기를 구현합니다. Zero-copy 직렬화와 스키마 진화를 지원하는 고성능 바이너리 포맷을 생성합니다.

## 🎯 목표
- `.fbs` 파일을 파싱하는 DSL 구현
- Zero-copy 직렬화를 위한 메모리 레이아웃 최적화
- 스키마 진화(Backward/Forward Compatibility) 지원
- 고성능 바이너리 직렬화/역직렬화

## 📁 프로젝트 구조
```
FlatBuffers/
├── src/
│   ├── FlatBuffers/
│   │   ├── Parser.hs          # .fbs 파일 파서 (Megaparsec)
│   │   ├── AST.hs             # FlatBuffers AST 정의
│   │   ├── Schema.hs          # 스키마 정의 및 검증
│   │   ├── CodeGen.hs         # Haskell 코드 생성기
│   │   ├── Serialize.hs       # FlatBuffers 직렬화
│   │   ├── MemoryLayout.hs    # 메모리 레이아웃 관리
│   │   └── Types.hs           # 기본 타입 정의
│   └── Main.hs                # CLI 메인 프로그램
├── examples/
│   ├── monster.fbs            # 예제 .fbs 파일
│   ├── schema_evolution.fbs   # 스키마 진화 예제
│   └── generated/             # 생성된 Haskell 코드
├── tests/
│   ├── ParserTest.hs          # 파서 테스트
│   ├── SchemaTest.hs          # 스키마 검증 테스트
│   ├── SerializeTest.hs       # 직렬화 테스트
│   └── PerformanceTest.hs     # 성능 테스트
├── benchmarks/
│   ├── SerializeBench.hs      # 직렬화 벤치마크
│   └── MemoryBench.hs         # 메모리 사용량 벤치마크
├── FlatBuffers.cabal          # Cabal 설정
└── TODO.md                    # 이 파일
```

## ✅ 구현 완료된 기능

### 1단계: 기본 타입 및 AST 정의 ✅
- [x] FlatBuffers 기본 타입 정의 (byte, ubyte, short, ushort, int, uint, float, double, bool, string)
- [x] Table, Struct, Union, Enum 등 AST 노드 정의
- [x] 간단한 AST 구조 (SimpleTypes.hs)
- [x] 스키마 버전 관리 구조

### 2단계: IDL 파서 구현 ✅
- [x] Megaparsec를 사용한 .fbs 문법 파서 (SimpleParser.hs)
- [x] Namespace 및 Include 문 파싱
- [x] Table 정의 파싱 (필드, 기본값, deprecated)
- [x] Struct 정의 파싱 (고정 크기 구조체)
- [x] Union 정의 파싱
- [x] Enum 정의 파싱
- [x] Root type 정의 파싱

### 3단계: 다중 언어 코드 생성기 구현 ✅
- [x] Haskell 코드 생성 (Record 타입, Type class, Enum)
- [x] C++ 코드 생성 (Struct, Class, Enum, Namespace, Union with std::variant)
- [x] C# 코드 생성 (Class, Interface, Enum, Properties, Abstract Union classes)
- [x] Python 코드 생성 (Dataclass, Class, Type hints, Union types)

### 4단계: 고급 코드 생성 기능 ✅
- [x] **예약어 검사**: 각 언어별 예약어 자동 변환
- [x] **C++ 전방 선언**: 의존성 순서로 타입 선언 (enum → struct/table → union)
- [x] **C# PascalCase**: 필드명 자동 변환 (`fieldName` → `FieldName`)
- [x] **Union 타입 처리**: 
  - C++: `std::variant<Type1, Type2>`
  - C#: Abstract class with derived classes
  - Python: `Type1 | Type2` union types
- [x] **문법 오류 방지**: 
  - 마지막 enum 값 콤마 제거
  - Record 필드 콤마 관리
  - C++ struct 의존성 순서 정렬

### 5단계: CLI 도구 구현 ✅
- [x] optparse-applicative를 사용한 CLI 인터페이스
- [x] 다중 언어 출력 지원 (`-l haskell|cpp|csharp|python`)
- [x] 출력 파일 지정 (`-o output.hs`)
- [x] 상세 출력 모드 (`-v`)

## 🔧 향후 구현 계획

### 6단계: FlatBuffers 직렬화
- [ ] Wire format 구현 (Little-endian, Offset-based)
- [ ] String table 및 Offset table 관리
- [ ] Zero-copy 역직렬화
- [ ] 메모리 정렬 최적화
- [ ] 압축 지원 (선택사항)

### 6단계: 스키마 진화 지원
- [ ] Backward compatibility 검증
- [ ] Forward compatibility 검증
- [ ] 필드 추가/제거 처리
- [ ] 타입 변경 처리
- [ ] Default value 처리

### 7단계: CLI 도구 구현
- [ ] optparse-applicative를 사용한 CLI 인터페이스
- [ ] 스키마 검증 모드
- [ ] 코드 생성 모드
- [ ] 성능 벤치마크 모드
- [ ] 스키마 비교 도구

### 8단계: 테스트 및 최적화
- [ ] 단위 테스트 작성
- [ ] 통합 테스트 작성
- [ ] 성능 벤치마크
- [ ] 메모리 프로파일링
- [ ] 스키마 진화 테스트

## 📚 사용할 라이브러리
- **파싱**: `megaparsec`, `text`
- **코드 생성**: `template-haskell`, `prettyprinter`
- **바이너리**: `bytestring`, `binary`, `cereal`
- **메모리**: `primitive`, `vector`
- **CLI**: `optparse-applicative`
- **테스트**: `hspec`, `quickcheck`
- **벤치마크**: `criterion`

## 🎯 예상 결과물
```haskell
-- monster.fbs에서 생성된 코드
data Monster = Monster
    { monsterPos :: Maybe Vec3
    , monsterMana :: Int16
    , monsterHp :: Int16
    , monsterName :: Maybe Text
    , monsterInventory :: Maybe (Vector Byte)
    , monsterColor :: Color
    , monsterWeapons :: Maybe (Vector Weapon)
    , monsterEquipped :: Maybe Equipment
    , monsterPath :: Maybe (Vector Vec3)
    } deriving (Show, Eq)

instance FlatBufferMessage Monster where
    buildMessage = buildMonster
    readMessage = readMonster
```

## 🔍 FlatBuffers 특징
- **Zero-copy**: 직렬화된 데이터를 직접 읽기
- **Schema Evolution**: 기존 데이터와 호환성 유지
- **Cross-platform**: 다양한 언어 지원
- **Memory Efficient**: 최소한의 메모리 사용
- **Fast**: 고속 직렬화/역직렬화

## 🔍 참고 자료
- [FlatBuffers Documentation](https://google.github.io/flatbuffers/)
- [FlatBuffers Schema Language](https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html)
- [Zero-copy Serialization](https://capnproto.org/news/2014-06-17-capnproto-flatbuffers-sbe.html)
