# Protobuf DSL Packet Generator

Haskell DSL을 사용하여 Protocol Buffers 스타일의 패킷 생성기를 구현한 프로젝트입니다. `.proto` 파일을 파싱하고, Haskell, C++, C# 등 다중 언어로 타입 안전한 코드를 생성합니다.

## 📋 프로젝트 개요

이 프로젝트는 Protocol Buffers의 IDL(Interface Definition Language)을 파싱하고, Haskell의 강력한 타입 시스템을 활용하여 다중 언어(Haskell, C++, C#)로 타입 안전한 코드를 자동 생성하는 DSL을 구현합니다. 사용자는 `.proto` 파일을 입력하면 원하는 언어의 타입 정의와 인터페이스를 생성할 수 있습니다.

## 🎯 주요 기능

- **`.proto` 파일 파싱**: Megaparsec를 사용한 강력한 파서
- **다중 언어 코드 생성**: Haskell, C++, C# 지원
- **타입 안전한 코드 생성**: 각 언어의 관례에 맞는 타입 생성
- **CLI 도구**: 사용자 친화적인 명령줄 인터페이스
- **유연한 출력 옵션**: 출력 디렉토리 및 파일명 지정
- **검증 시스템**: 파싱된 구조의 유효성 검사
- **확장 가능한 구조**: 새로운 타입과 기능 추가 용이

## 📁 프로젝트 구조

```
Protobuf/
├── src/
│   ├── Protobuf/
│   │   ├── SimpleTypes.hs      # 기본 타입 정의
│   │   ├── SimpleParser.hs     # .proto 파일 파서
│   │   ├── SimpleCodeGen.hs    # Haskell 코드 생성기
│   │   ├── Types.hs            # 고급 타입 시스템 (개발 중)
│   │   ├── AST.hs              # GADT 기반 AST (개발 중)
│   │   ├── Parser.hs           # 고급 파서 (개발 중)
│   │   ├── CodeGen.hs          # Template Haskell 코드 생성 (개발 중)
│   │   └── Serialize.hs        # 바이너리 직렬화 (개발 중)
│   ├── SimpleMain.hs           # CLI 메인 프로그램
│   └── Main.hs                 # 고급 CLI (개발 중)
├── examples/
│   └── person.proto            # 예제 .proto 파일
├── tests/
│   ├── ParserTest.hs           # 파서 테스트
│   ├── CodeGenTest.hs          # 코드 생성 테스트
│   ├── SerializeTest.hs        # 직렬화 테스트
│   └── Test.hs                 # 통합 테스트
├── Protobuf.cabal              # Cabal 설정
├── TODO.md                     # 구현 계획
└── README.md                   # 이 파일
```

## 🔧 구현된 기능

### 1. 기본 타입 시스템 (`SimpleTypes.hs`)

```haskell
-- Protobuf 스칼라 타입
data ProtobufScalarType
    = DoubleType | FloatType | Int32Type | Int64Type
    | UInt32Type | UInt64Type | SInt32Type | SInt64Type
    | Fixed32Type | Fixed64Type | SFixed32Type | SFixed64Type
    | BoolType | StringType | BytesType

-- 필드 규칙
data FieldRule = Required | Optional | Repeated

-- 메시지 정의
data Message = Message
    { messageName :: Text
    , messageFields :: [Field]
    }
```

### 2. 파서 시스템 (`SimpleParser.hs`)

- **Megaparsec 기반**: 강력한 파서 컴비네이터
- **문법 지원**: message, enum, service, field 파싱
- **에러 처리**: 상세한 파싱 에러 메시지
- **타입 안전성**: 파싱 결과를 타입 안전한 구조로 변환

```haskell
-- 메시지 파싱 예시
parseMessage :: Parser Message
parseMessage = do
    keyword "message"
    name <- identifier
    char '{'
    fields <- many parseField
    char '}'
    return $ Message name fields
```

### 3. 코드 생성기 (`SimpleCodeGen.hs`)

- **다중 언어 지원**: Haskell, C++, C# 코드 생성
- **Haskell 타입 생성**: 메시지를 Haskell 데이터 타입으로 변환
- **C++ 헤더 생성**: C++ struct, enum class, 클래스 생성
- **C# 클래스 생성**: C# class, enum, interface 생성
- **열거형 생성**: Protobuf enum을 각 언어의 enum으로 변환
- **서비스 생성**: RPC 서비스를 각 언어의 인터페이스로 변환
- **타입 매핑**: Protobuf 타입을 각 언어의 적절한 타입으로 매핑
- **네임스페이스 지원**: package 선언을 각 언어의 네임스페이스로 변환

```haskell
-- 생성되는 Haskell 코드 예시
data Person = Person
    { name :: Text
    , age :: Int32
    , email :: Text
    } deriving (Show, Eq, Generic)
```

```cpp
// 생성되는 C++ 코드 예시
struct Person {
  std::string name;
  int32_t age;
  std::string email;
};
```

```csharp
// 생성되는 C# 코드 예시
public class Person
{
  public string name { get; set; }
  public int age { get; set; }
  public string email { get; set; }
}
```

### 4. CLI 도구 (`SimpleMain.hs`)

- **optparse-applicative**: 사용자 친화적인 CLI 인터페이스
- **다중 언어 지원**: Haskell, C++, C# 출력 언어 선택
- **유연한 출력 옵션**: 출력 디렉토리 및 파일명 지정
- **자동 디렉토리 생성**: 출력 디렉토리가 없으면 자동 생성
- **파일 I/O**: .proto 파일 읽기 및 각 언어별 파일 생성
- **에러 처리**: 파싱 및 파일 처리 에러 관리
- **상세 옵션**: verbose 모드, 출력 파일 지정

## 🚀 사용법

### 빌드

```bash
cd /home/milman2/haskell/Packet/Protobuf
cabal build
```

### 실행

```bash
# 기본 사용 (Haskell 코드 생성)
./protobuf-generator person.proto

# 언어 지정
./protobuf-generator person.proto -l haskell    # Haskell (.hs)
./protobuf-generator person.proto -l cpp        # C++ (.hpp)
./protobuf-generator person.proto -l csharp     # C# (.cs)

# 출력 파일 지정
./protobuf-generator person.proto -o Person.hs

# 출력 디렉토리 지정
./protobuf-generator person.proto -d output

# 상세 출력
./protobuf-generator person.proto -v
```

### 예제

```bash
# Haskell 코드 생성
./protobuf-generator examples/person.proto -l haskell -v

# C++ 헤더 파일 생성
./protobuf-generator examples/person.proto -l cpp -d cpp_output -v

# C# 클래스 파일 생성
./protobuf-generator examples/person.proto -l csharp -d csharp_output -v
```

## 📝 예제 .proto 파일

```protobuf
syntax = "proto3";

package tutorial;

message Person {
  string name = 1;
  int32 id = 2;
  string email = 3;

  enum PhoneType {
    MOBILE = 0;
    HOME = 1;
    WORK = 2;
  }

  message PhoneNumber {
    string number = 1;
    PhoneType type = 2;
  }

  repeated PhoneNumber phones = 4;
}

service AddressBookService {
  rpc AddPerson(Person) returns (int32);
  rpc GetPerson(int32) returns (Person);
  rpc ListPeople(Empty) returns (AddressBook);
}
```

## 🔍 생성되는 코드 예시

### Haskell 코드

```haskell
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

data PhoneNumber = PhoneNumber {
  number :: Text ,
  type :: PhoneType ,
} deriving (Show, Eq, Generic)

data PhoneType = MOBILE | HOME | WORK deriving (Show, Eq, Generic)

data Person = Person {
  name :: Text ,
  id :: Int32 ,
  email :: Text ,
  phones :: [PhoneNumber] ,
} deriving (Show, Eq, Generic)

class AddressBookService m where
  AddPerson :: Person -> m Int32
  GetPerson :: Int32 -> m Person
  ListPeople :: Empty -> m AddressBook
```

### C++ 코드

```cpp
#pragma once
#include <string>
#include <vector>
#include <map>
#include <cstdint>

namespace tutorial {

struct PhoneNumber {
  std::string number;
  PhoneType type;
};

enum class PhoneType {
  MOBILE = 0,
  HOME = 1,
  WORK = 2,
};

struct Person {
  std::string name;
  int32_t id;
  std::string email;
  std::vector<PhoneNumber> phones;
};

class AddressBookService {
public:
  virtual int32_t AddPerson(Person input) = 0;
  virtual Person GetPerson(int32_t input) = 0;
  virtual AddressBook ListPeople(Empty input) = 0;
};

} // namespace
```

### C# 코드

```csharp
using System;
using System.Collections.Generic;

namespace tutorial
{
public class PhoneNumber
{
  public string number { get; set; }
  public PhoneType type { get; set; }
}

public enum PhoneType
{
  MOBILE = 0,
  HOME = 1,
  WORK = 2,
}

public class Person
{
  public string name { get; set; }
  public int id { get; set; }
  public string email { get; set; }
  public List<PhoneNumber> phones { get; set; }
}

public interface AddressBookService
{
  int AddPerson(Person input);
  Person GetPerson(int input);
  AddressBook ListPeople(Empty input);
}

}
```

## 🧪 테스트

```bash
# 테스트 실행
cabal test

# 개별 테스트 모듈 실행
cabal run protobuf-tests
```

## 📚 사용된 라이브러리

- **megaparsec**: 강력한 파서 컴비네이터
- **text**: 효율적인 텍스트 처리
- **optparse-applicative**: CLI 인터페이스
- **filepath**: 파일 경로 처리
- **directory**: 디렉토리 생성 및 관리
- **template-haskell**: 메타프로그래밍 (고급 기능)
- **bytestring**: 바이너리 데이터 처리 (직렬화)
- **binary**: 바이너리 직렬화 (직렬화)

## 🔄 개발 상태

### ✅ 완성된 기능
- [x] 기본 타입 시스템
- [x] 파서 시스템
- [x] 다중 언어 코드 생성기 (Haskell, C++, C#)
- [x] CLI 도구 (언어 선택, 출력 옵션)
- [x] 파일 생성 위치 지정
- [x] 자동 디렉토리 생성
- [x] 네임스페이스/패키지 지원
- [x] 중첩된 타입 지원
- [x] 서비스/RPC 인터페이스 생성
- [x] 예제 파일
- [x] Git 무시 설정

### 🔄 개발 중인 기능
- [ ] 테스트 구현
- [ ] 고급 타입 시스템 (GADT)
- [ ] Template Haskell 코드 생성
- [ ] 바이너리 직렬화
- [ ] 에러 처리 개선

### 📋 향후 계획
- [ ] 성능 최적화
- [ ] 더 많은 Protobuf 기능 지원 (oneof, extensions 등)
- [ ] 추가 언어 지원 (Java, Python, Go 등)
- [ ] 바이너리 직렬화/역직렬화 구현
- [ ] 문서화 개선
- [ ] CI/CD 설정

## 🤝 기여하기

1. 이슈 생성 또는 기존 이슈 확인
2. 포크 및 브랜치 생성
3. 변경사항 구현
4. 테스트 작성 및 실행
5. 풀 리퀘스트 생성

## 📄 라이선스

MIT License

## 🔗 관련 프로젝트

- [FlatBuffers DSL](../FlatBuffers/) - FlatBuffers 스타일 패킷 생성기
- [Cap'n Proto DSL](../CapnProto/) - Cap'n Proto 스타일 패킷 생성기

---

*이 프로젝트는 Haskell의 강력한 타입 시스템과 DSL 기능을 활용하여 현대적인 패킷 생성기를 구현하는 것을 목표로 합니다.*
