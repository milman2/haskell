#include <iostream>
#include <string>
#include <cstring>

extern "C" {
    // C++ 함수들을 C 인터페이스로 래핑
    
    int add_numbers(int a, int b) {
        return a + b;
    }
    
    int multiply_numbers(int a, int b) {
        return a * b;
    }
    
    // 문자열을 반환하는 함수 (메모리 관리 주의)
    char* get_greeting() {
        std::string greeting = "Hello from C++!";
        char* result = new char[greeting.length() + 1];
        std::strcpy(result, greeting.c_str());
        return result;
    }
    
    // 메모리 해제 함수
    void free_string(char* str) {
        delete[] str;
    }
    
    // 더 복잡한 예제: 클래스 기반 기능
    class Calculator {
    private:
        int value;
    public:
        Calculator(int initial = 0) : value(initial) {}
        
        int add(int n) { return value += n; }
        int multiply(int n) { return value *= n; }
        int getValue() { return value; }
        void reset() { value = 0; }
    };
    
    // Calculator 클래스를 C 인터페이스로 래핑
    void* create_calculator(int initial) {
        return new Calculator(initial);
    }
    
    int calculator_add(void* calc, int n) {
        Calculator* c = static_cast<Calculator*>(calc);
        return c->add(n);
    }
    
    int calculator_multiply(void* calc, int n) {
        Calculator* c = static_cast<Calculator*>(calc);
        return c->multiply(n);
    }
    
    int calculator_get_value(void* calc) {
        Calculator* c = static_cast<Calculator*>(calc);
        return c->getValue();
    }
    
    void calculator_reset(void* calc) {
        Calculator* c = static_cast<Calculator*>(calc);
        c->reset();
    }
    
    void destroy_calculator(void* calc) {
        Calculator* c = static_cast<Calculator*>(calc);
        delete c;
    }
}
