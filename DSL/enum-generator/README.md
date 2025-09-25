# Enum DSL Generator

Haskell로 작성된 DSL(Domain Specific Language) 프로젝트로, 간단한 enum 정의 파일을 파싱하여 C++와 C#용 enum 코드를 자동 생성합니다.

## 🚀 기능

- **DSL 파싱**: 간단한 문법으로 enum 정의
- **다중 언어 지원**: C++와 C# 코드 동시 생성
- **타입 안전성**: 다양한 정수 타입 지원 (uint8, uint16, uint32, uint64, int8, int16, int32, int64)
- **자동 변환 함수**: toString/fromString 함수 자동 생성
- **명시적 값 지정**: enum 값에 명시적 숫자 할당 지원

## 📝 DSL 문법

```haskell
enum EnumName : type {
    VALUE1,
    VALUE2 = 5,
    VALUE3,
}
```

### 지원 타입
- `uint8`, `uint16`, `uint32`, `uint64`
- `int8`, `int16`, `int32`, `int64`

### 예제
```haskell
enum Color : uint8 {
    RED,
    GREEN,
    BLUE,
}

enum WeekOfDay : uint8 {
    MONDAY = 0,
    TUESDAY,
    WEDNESDAY,
    THURSDAY,
    FRIDAY,
    SATURDAY,
    SUNDAY,
}

enum Status : int32 {
    PENDING = 0,
    RUNNING = 1,
    COMPLETED = 2,
    FAILED = 3,
    CANCELLED = 4,
}
```

## 🛠️ 설치 및 사용

### 요구사항
- Haskell Stack
- GHC 9.10.3+

### 빌드
```bash
stack build
```

### 사용법
```bash
# 기본 사용법 (현재 디렉토리에 파일 생성)
stack exec enum-generator-exe input.txt

# 출력 디렉토리 지정
stack exec enum-generator-exe input.txt output/
```

### 예제 실행
```bash
stack exec enum-generator-exe example.txt
```

## 📁 프로젝트 구조

```
enum-generator/
├── src/
│   ├── EnumDSL.hs      # DSL 파서 및 코드 생성기
│   └── Lib.hs          # 라이브러리 모듈
├── app/
│   └── Main.hs         # 메인 실행 파일
├── example.txt         # 예제 입력 파일
├── package.yaml        # Stack 프로젝트 설정
└── README.md           # 이 파일
```

## 🔧 생성되는 코드

### C++ 코드
- `enum class` 정의
- `toString()` 함수 (enum → string)
- `fromString()` 함수 (string → enum)
- 적절한 C++ 타입 매핑 (uint8 → std::uint8_t)

### C# 코드
- `public enum` 정의
- Extension 클래스
- `ToString()` 확장 메서드
- `FromString()` 정적 메서드
- 적절한 C# 타입 매핑 (uint8 → byte)

## 🎯 사용 사례

1. **API 정의**: REST API의 상태 코드나 에러 코드 정의
2. **게임 개발**: 게임 상태, 아이템 타입, 캐릭터 클래스 등
3. **시스템 프로그래밍**: 프로토콜 정의, 시스템 상태 등
4. **크로스 플랫폼 개발**: C++와 C# 간 공통 enum 정의

## 🧪 테스트

```bash
# 예제 파일로 테스트
stack exec enum-generator-exe example.txt

# 생성된 파일 확인
ls -la *.hpp *.cs
```

## 📚 기술 스택

- **Haskell**: 함수형 프로그래밍 언어
- **Parsec**: 파서 컴비네이터 라이브러리
- **Text**: 효율적인 텍스트 처리
- **Stack**: Haskell 빌드 도구

## 🤝 기여

1. 이슈 리포트
2. 기능 제안
3. 코드 개선
4. 문서화 개선

## 📄 라이선스

BSD-3-Clause

## 🔮 향후 계획

- [ ] JSON 출력 지원
- [ ] Rust 코드 생성
- [ ] Python 코드 생성
- [ ] 더 복잡한 DSL 문법 지원
- [ ] IDE 플러그인 개발
- [ ] CI/CD 통합

---

**Haskell DSL로 코드 생성의 힘을 경험해보세요!** 🚀