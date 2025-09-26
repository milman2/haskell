# Haskell-C++ FFI (Foreign Function Interface) 예제

이 프로젝트는 Haskell에서 C++ 함수를 호출하는 방법을 보여주는 예제입니다.

## 📁 프로젝트 구조

```
FFI/
├── haskell-to-cpp.hs      # Haskell에서 C++ 함수 호출
├── cpp_functions.cpp      # C++ 구현
├── cpp_functions.h        # C++ 헤더
├── Makefile              # 빌드 설정
├── FFI.cabal            # Cabal 설정
└── README.md            # 이 파일
```

## 🚀 빌드 및 실행

### 방법 1: Makefile 사용

```bash
# Haskell에서 C++ 호출 예제 빌드 및 실행 (✅ 작동함)
make haskell-to-cpp
./haskell-to-cpp

# 정리
make clean
```

**현재 상태**: 
- ✅ **Haskell → C++ 호출**: 완벽하게 작동

### 방법 2: Cabal 사용

```bash
# 빌드
cabal build

# 실행
cabal run haskell-to-cpp
cabal run cpp-calls-haskell
```

### 방법 3: 수동 빌드

```bash
# C++ 객체 파일 생성
g++ -fPIC -O2 -std=c++11 -c cpp_functions.cpp -o cpp_functions.o

# Haskell에서 C++ 호출 예제
ghc -fPIC -O2 -o haskell-to-cpp haskell-to-cpp.hs cpp_functions.o

# C++에서 Haskell 호출 예제
ghc -fPIC -O2 -c haskell-functions.hs -o haskell-functions.o
ghc -fPIC -O2 -o cpp-calls-haskell cpp_calls_haskell.cpp haskell-functions.o -lstdc++
```

## 📋 예제 설명

### 1. Haskell → C++ 호출 ✅ **작동함**

**haskell-to-cpp.hs**에서 C++ 함수들과 클래스를 호출합니다:

- `addNumbers`: 두 정수를 더함
- `multiplyNumbers`: 두 정수를 곱함
- `getGreeting`: C++에서 문자열 반환
- `Calculator` 클래스: 상태를 가진 계산기 객체

**주요 특징:**
- `foreign import ccall`로 C++ 함수 선언
- `extern "C"`로 C++ 함수를 C 인터페이스로 래핑
- 메모리 관리 (문자열 할당/해제, 객체 생성/소멸)
- C++ 클래스의 상태 관리

**실행 결과:**
```
=== Haskell에서 C++ 함수 호출 테스트 ===
10 + 20 = 30
5 * 6 = 30
C++에서 온 인사: Hello from C++!

=== Calculator 클래스 테스트 ===
Calculator 생성 (초기값: 10)
현재 값: 10
10 + 5 = 15
15 * 3 = 45
최종 값: 45
리셋 후 값: 0
Calculator 정리 완료
```


## 🔧 주요 기술적 요소

### 1. 타입 매핑

| Haskell | C++ | 설명 |
|---------|-----|------|
| `Int` | `int` | 정수 |
| `String` | `char*` | 문자열 |
| `IO a` | 함수 호출 | 부작용 |
| `Ptr a` | `void*` | 포인터 |

### 2. 메모리 관리

- **Haskell → C++**: C++에서 할당한 메모리는 C++에서 해제
- **C++ → Haskell**: Haskell에서 할당한 메모리는 Haskell에서 해제
- `hs_free_fun_ptr`로 Haskell 포인터 해제

### 3. 런타임 관리

```cpp
// 초기화
hs_init(nullptr, nullptr);
hs_add_root(__stginit_HaskellFunctions);

// 정리
hs_exit();
```

## ⚠️ 주의사항

1. **메모리 관리**: 할당과 해제가 같은 언어에서 이루어져야 함
2. **타입 안전성**: FFI는 타입 안전성을 보장하지 않음
3. **예외 처리**: C++ 예외가 Haskell으로 전파되지 않음
4. **스레드 안전성**: Haskell 런타임의 스레드 모델 고려 필요

## 🎯 실제 사용 사례

### 1. 성능이 중요한 부분을 C++로 구현
```haskell
-- Haskell에서 C++의 고성능 알고리즘 사용
foreign import ccall "fast_sort" c_fastSort :: Ptr CInt -> CInt -> IO ()
```

### 2. 기존 C++ 라이브러리 활용
```haskell
-- OpenCV, Eigen 등 C++ 라이브러리 사용
foreign import ccall "opencv_function" c_opencvFunc :: CString -> IO CInt
```

### 3. 시스템 프로그래밍
```haskell
-- 시스템 콜이나 하드웨어 접근
foreign import ccall "system_call" c_systemCall :: CInt -> IO CInt
```

## 📚 추가 자료

- [Haskell FFI Cookbook](https://wiki.haskell.org/FFI_cook_book)
- [GHC User's Guide - FFI](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi.html)
- [Real World Haskell - FFI](http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html)

## 🐛 문제 해결

### 컴파일 오류
- `extern "C"` 래핑 확인
- 헤더 파일 경로 확인
- 링크 라이브러리 확인

### 런타임 오류
- 메모리 누수 확인
- Haskell 런타임 초기화 확인
- 타입 변환 확인

## 🎯 결론

### ✅ **성공적으로 작동하는 것:**
- **Haskell → C++ 호출**: 완벽하게 작동하며, 성능이 중요한 부분을 C++로 구현하고 고수준 로직을 Haskell로 작성하는 하이브리드 개발에 매우 유용합니다.

### 💡 **권장사항:**
**Haskell에서 C++ 호출** 방식을 사용하는 것이 간단하고 안정적입니다. 이는 이미 완벽하게 작동하며, 대부분의 실제 사용 사례에서 충분합니다.

---

*이 예제는 Haskell과 C++ 간의 FFI 사용법을 보여줍니다. 실제 프로덕션에서는 더 신중한 메모리 관리와 오류 처리가 필요합니다.*
