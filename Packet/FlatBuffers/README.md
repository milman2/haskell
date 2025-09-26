# FlatBuffers DSL Packet Generator

Haskell로 구현된 FlatBuffers 스타일의 IDL(Interface Definition Language) 기반 패킷 생성기입니다. `.fbs` 파일을 파싱하여 다중 언어(Haskell, C++, C#, Python)로 타입 안전한 코드를 생성합니다.

## 🎯 주요 기능

### 1. FlatBuffers 스키마 파싱
- **`.fbs` 파일 지원**: FlatBuffers 스키마 언어 완전 지원
- **Table, Struct, Union, Enum**: 모든 FlatBuffers 타입 지원
- **Namespace 지원**: 네임스페이스 기반 코드 생성
- **기본값 및 Deprecated**: 필드 기본값과 deprecated 속성 지원

### 2. 다중 언어 코드 생성
- **Haskell**: 타입 안전한 데이터 타입 및 레코드 생성
- **C++**: struct, enum class, getter/setter 함수 포함
- **C#**: class, enum, PascalCase 속성 생성
- **Python**: dataclass, enum 기반 코드 생성

### 3. 예약어 검사 및 안전한 이름 변환
- **자동 예약어 감지**: 각 언어의 예약어와 충돌하는 필드명 자동 감지
- **안전한 이름 변환**: 예약어 충돌 시 `Field` 접미사 자동 추가
- **언어별 최적화**: 각 언어의 네이밍 컨벤션에 맞는 변환

### 4. CLI 도구
- **사용자 친화적 인터페이스**: optparse-applicative 기반 CLI
- **유연한 출력 옵션**: 파일 또는 디렉토리 지정 가능
- **Verbose 모드**: 상세한 처리 과정 출력

## 📁 프로젝트 구조

```
FlatBuffers/
├── src/
│   ├── FlatBuffers/
│   │   ├── SimpleTypes.hs      # FlatBuffers AST 타입 정의
│   │   ├── SimpleParser.hs     # .fbs 파일 파서 (Megaparsec)
│   │   └── SimpleCodeGen.hs    # 다중 언어 코드 생성기
│   └── SimpleMain.hs           # CLI 메인 프로그램
├── examples/
│   ├── monster.fbs             # 예제 FlatBuffers 스키마
│   ├── schema_evolution.fbs    # 스키마 진화 예제
│   └── reserved_words_test.fbs # 예약어 테스트 파일
├── generated/                  # 생성된 코드 출력 디렉토리
├── FlatBuffers.cabal           # Cabal 설정
└── README.md                   # 이 파일
```

## 🚀 사용법

### 기본 사용법

```bash
# Haskell 코드 생성 (기본값)
./flatbuffers-generator examples/monster.fbs

# 특정 언어 지정
./flatbuffers-generator examples/monster.fbs -l cpp
./flatbuffers-generator examples/monster.fbs -l csharp
./flatbuffers-generator examples/monster.fbs -l python

# 출력 파일 지정
./flatbuffers-generator examples/monster.fbs -o output.hs

# 출력 디렉토리 지정
./flatbuffers-generator examples/monster.fbs -d output/

# 상세 출력 모드
./flatbuffers-generator examples/monster.fbs -v
```

### CLI 옵션

- `INPUT`: 입력 `.fbs` 파일 경로
- `-l, --language LANG`: 출력 언어 (haskell, cpp, csharp, python)
- `-o, --output FILE`: 출력 파일 경로
- `-d, --output-dir DIR`: 출력 디렉토리 (기본값: generated/)
- `-v, --verbose`: 상세 출력 모드

## 📝 예제

### 입력 파일 (monster.fbs)

```flatbuffers
namespace MyGame.Sample;

enum Color:byte { Red = 0, Green, Blue = 2 }

union Equipment { Weapon, Armor }

struct Vec3 {
  x:float;
  y:float;
  z:float;
}

table Monster {
  pos:Vec3;
  mana:short = 150;
  hp:short = 100;
  name:string;
  friendly:bool = false (deprecated);
  inventory:[ubyte];
  color:Color = Blue;
  weapons:[Weapon];
  equipped:Equipment;
  path:[Vec3];
}

table Weapon {
  name:string;
  damage:short;
}

root_type Monster;
```

### 생성되는 코드 예시

#### Haskell
```haskell
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Color = Red | Green | Blue deriving (Show, Eq, Generic, Enum, Bounded)

data Vec3 = Vec3 {
  x :: Float ,
  y :: Float ,
  z :: Float ,
} deriving (Show, Eq, Generic)

data Monster = Monster {
  pos :: Vec3 ,
  mana :: Int16 ,
  hp :: Int16 ,
  name :: Text ,
  friendly :: Bool ,
  inventory :: Vector Word8 ,
  color :: Color ,
  weapons :: Vector Weapon ,
  equipped :: Equipment ,
  path :: Vector Vec3 ,
} deriving (Show, Eq, Generic)
```

#### C++
```cpp
#pragma once
#include <string>
#include <vector>
#include <cstdint>

namespace MyGame.Sample {

enum class Color {
  Red = 0,
  Green,
  Blue = 2,
};

struct Monster {
public:
  Vec3 & getPos() { return pos; }
  const Vec3 & getPos() const { return pos; }
  int16_t & getMana() { return mana; }
  const int16_t & getMana() const { return mana; }
  // ... 더 많은 getter/setter 함수들

  void setPos(const Vec3& posValue) { pos = posValue; }
  void setMana(const int16_t& manaValue) { mana = manaValue; }
  // ... 더 많은 setter 함수들

private:
  Vec3 pos;
  int16_t mana;
  // ... 더 많은 필드들
};

} // namespace
```

#### C#
```csharp
using System;
using System.Collections.Generic;

namespace MyGame.Sample
{
public enum Color
{
  Red = 0,
  Green,
  Blue = 2,
}

public class Monster
{
  public Vec3 Pos { get; set; }
  public short Mana { get; set; }
  public short Hp { get; set; }
  public string Name { get; set; }
  public bool Friendly { get; set; }
  public List<byte> Inventory { get; set; }
  public Color Color { get; set; }
  public List<Weapon> Weapons { get; set; }
  public Equipment Equipped { get; set; }
  public List<Vec3> Path { get; set; }
}
}
```

#### Python
```python
from dataclasses import dataclass
from typing import List
from enum import Enum

class Color(Enum):
  Red = 0
  Green
  Blue = 2

@dataclass
class Monster:
  pos: Vec3
  mana: int
  hp: int
  name: str
  friendly: bool
  inventory: List[int]
  color: Color
  weapons: List[Weapon]
  equipped: Equipment
  path: List[Vec3]
```

## 🔧 예약어 검사 기능

각 언어의 예약어와 충돌하는 필드명을 자동으로 안전한 이름으로 변환합니다.

### 예약어 변환 예시

**입력 (.fbs):**
```flatbuffers
struct ReservedTest {
  type:byte;        // Haskell 예약어
  class:short;      // Haskell, C++, C# 예약어
  if:string;        // 모든 언어 예약어
  for:Color;        // 모든 언어 예약어
}
```

**Haskell 출력:**
```haskell
data ReservedTest = ReservedTest {
  typeField :: Int8 ,    -- type -> typeField
  classField :: Int16 ,  -- class -> classField
  ifField :: Text ,      -- if -> ifField
  for :: Color ,         -- for는 Haskell에서 예약어가 아님
} deriving (Show, Eq, Generic)
```

**C# 출력:**
```csharp
public struct ReservedTest
{
  public sbyte TypeField { get; set; }    // type -> TypeField
  public short ClassField { get; set; }   // class -> ClassField
  public string IfField { get; set; }     // if -> IfField
  public Color ForField { get; set; }     // for -> ForField
}
```

## 🛠️ 빌드 및 설치

### 요구사항
- GHC 9.10.2 이상
- Cabal 3.0 이상

### 빌드
```bash
cd FlatBuffers
cabal build
```

### 실행
```bash
./dist-newstyle/build/x86_64-linux/ghc-9.10.2/FlatBuffers-0.1.0.0/x/flatbuffers-generator/build/flatbuffers-generator/flatbuffers-generator examples/monster.fbs -l haskell
```

## 📚 사용된 라이브러리

- **megaparsec**: 파서 콤비네이터
- **optparse-applicative**: CLI 인터페이스
- **text**: 유니코드 텍스트 처리
- **filepath**: 파일 경로 처리
- **directory**: 디렉토리 조작

## 🎯 FlatBuffers 특징

- **Zero-copy**: 직렬화된 데이터를 직접 읽기
- **Schema Evolution**: 기존 데이터와 호환성 유지
- **Cross-platform**: 다양한 언어 지원
- **Memory Efficient**: 최소한의 메모리 사용
- **Fast**: 고속 직렬화/역직렬화

## 🔍 참고 자료

- [FlatBuffers Documentation](https://google.github.io/flatbuffers/)
- [FlatBuffers Schema Language](https://google.github.io/flatbuffers/flatbuffers_guide_writing_schema.html)
- [Zero-copy Serialization](https://capnproto.org/news/2014-06-17-capnproto-flatbuffers-sbe.html)

## 📄 라이선스

MIT License
