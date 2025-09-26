# Makefile for Haskell-C++ FFI examples

# 컴파일러 설정
GHC = ghc
GPP = g++
CFLAGS = -fPIC -O2
CXXFLAGS = -fPIC -O2 -std=c++11

# Haskell 설정
HASKELL_FLAGS = -fPIC -O2 -threaded

# 타겟들
HASKELL_TO_CPP = haskell-to-cpp

# 기본 타겟
all: $(HASKELL_TO_CPP)

# Haskell에서 C++ 호출
$(HASKELL_TO_CPP): haskell-to-cpp.hs cpp_functions.o
	$(GHC) $(HASKELL_FLAGS) -o $@ $< cpp_functions.o -lstdc++

cpp_functions.o: cpp_functions.cpp cpp_functions.h
	$(GPP) $(CXXFLAGS) -c $< -o $@


# 정리
clean:
	rm -f *.o *.hi $(HASKELL_TO_CPP)

# 테스트
test: all
	@echo "Testing Haskell to C++..."
	./$(HASKELL_TO_CPP)
	@echo "Testing C++ to Haskell..."
	./$(CPP_TO_HASKELL)

.PHONY: all clean test
