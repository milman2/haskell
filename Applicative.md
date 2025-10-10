# Applicative
- **함수가 들어 있는 컨텍스트에 값을 적용할 수 있는** 타입 클래스
## 기본 정의
```hs
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```
- pure: 값을 컨텍스트로 감싸기
- <*>: 함수가 들어있는 컨텍스트에 값을 적용