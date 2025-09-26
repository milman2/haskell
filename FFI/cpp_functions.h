#ifndef CPP_FUNCTIONS_H
#define CPP_FUNCTIONS_H

#ifdef __cplusplus
extern "C" {
#endif

// 기본 수학 함수들
int add_numbers(int a, int b);
int multiply_numbers(int a, int b);
char* get_greeting();
void free_string(char* str);

// Calculator 클래스 관련 함수들
void* create_calculator(int initial);
int calculator_add(void* calc, int n);
int calculator_multiply(void* calc, int n);
int calculator_get_value(void* calc);
void calculator_reset(void* calc);
void destroy_calculator(void* calc);

#ifdef __cplusplus
}
#endif

#endif // CPP_FUNCTIONS_H
