# Types - 사용자 정의 타입

## 개요

Haskell에서 사용자 정의 타입은 **Algebraic datatypes (대수적 자료 유형)** 또는 단순히 **자료 유형**이라고 불립니다. 이는 프로그래머가 자신만의 복합 타입을 정의할 수 있게 해주는 강력한 기능입니다.

## 기본 개념

### 타입 생성자 vs 데이터 생성자

Haskell에서는 다음 명명 규칙을 따릅니다:
- **타입 이름**과 **데이터 생성자 이름**은 대문자로 시작
- **타입 변수**만 소문자로 시작
- 타입과 값을 혼동하면 안 됩니다!

## 구체적인 예시: Tree 타입

```haskell
data Tree a = Leaf a
            | Node a (Tree a) (Tree a)
```

### 구성 요소 분석

- **`Tree`**: 타입 생성자 (Type constructor)
- **`a`**: 타입 변수 (Type variable)
- **`Tree a`**: 타입 (Type)
- **`Leaf`**: 데이터 생성자 (Data constructor)
- **`Node`**: 데이터 생성자 (Data constructor)

### Tree 타입의 의미

이 정의는 다음과 같은 의미를 가집니다:
- `Tree a`는 `a` 타입의 값을 저장하는 트리입니다
- `Leaf a`: 리프 노드로, 값 `a`를 직접 저장
- `Node a (Tree a) (Tree a)`: 내부 노드로, 값 `a`와 두 개의 서브트리를 가짐

## 일반적인 문법

```haskell
data D a₁ ... aₖ = C₁ T₁,₁ ... T₁,k₁
                  | ...
                  | Cₘ Tₘ,₁ ... Tₘ,kₘ
```

### 구성 요소 설명

- **`D`**: 타입 생성자 (Type constructor)
- **`a₁ ... aₖ`**: 타입 인자 변수 (Type argument variables)
- **`C₁, ..., Cₘ`**: 데이터 생성자 (Data constructors)
- **`T₁,₁ ... T₁,k₁, ..., Tₘ,₁ ... Tₘ,kₘ`**: 데이터 생성자 인자 타입 (Data constructor argument types)

## 실제 사용 예시

### Tree 타입 사용법

```haskell
-- 리프 노드 생성
leaf1 :: Tree Int
leaf1 = Leaf 42

-- 내부 노드 생성
tree1 :: Tree String
tree1 = Node "root" (Leaf "left") (Leaf "right")

-- 더 복잡한 트리
tree2 :: Tree Int
tree2 = Node 1 
           (Node 2 (Leaf 3) (Leaf 4))
           (Node 5 (Leaf 6) (Leaf 7))
```

### 패턴 매칭과 함께 사용

```haskell
-- 트리의 높이 계산
treeHeight :: Tree a -> Int
treeHeight (Leaf _) = 1
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

-- 트리의 모든 값 수집
treeValues :: Tree a -> [a]
treeValues (Leaf x) = [x]
treeValues (Node x left right) = [x] ++ treeValues left ++ treeValues right
```

## 레코드형 타입 (Record Types)

레코드형 타입은 필드에 이름을 부여하여 데이터를 더 명확하고 접근하기 쉽게 만드는 방법입니다.

### 기본 문법

```haskell
data TypeName = ConstructorName {field1 :: Type1, field2 :: Type2, ...}
```

### Vector2D 예시

```haskell
-- 레코드형 Record types
-- data Vector2D = MakeVector Double Double  -- 일반적인 방식
data Vector2D = MakeVector {x :: Double, y :: Double}  -- 레코드형 방식

v1 :: Vector2D
v1 = MakeVector (-1) 1

xy1 :: (Double, Double)
xy1 = (x v1, y v1) -- (-1, 1)
```

### 레코드형의 장점

1. **자동 필드 접근자**: 각 필드에 대한 접근 함수가 자동으로 생성됩니다
   ```haskell
   x :: Vector2D -> Double  -- x 필드 접근자
   y :: Vector2D -> Double  -- y 필드 접근자
   ```

   **⚠️ 필드 이름 충돌 주의사항**: 서로 다른 타입이 같은 필드 이름을 가질 때는 주의가 필요합니다.
   
   ```haskell
   data Point2D = Point2D {x :: Double, y :: Double}
   data Point3D = Point3D {x :: Double, y :: Double, z :: Double}
   
   -- 이 경우 x, y 함수가 두 타입 모두에 대해 생성되어 충돌 발생!
   -- 컴파일 에러: Ambiguous occurrence 'x'
   ```
   
   **해결 방법들**:
   
   a) **타입 어노테이션 사용** (실제로는 제한적):
   ```haskell
   point2d :: Point2D
   point2d = Point2D 1.0 2.0
   
   -- 이 방법은 실제로는 작동하지 않습니다!
   -- xCoord = x point2d :: Double  -- 여전히 모호함
   
   -- 올바른 방법: 함수 자체에 타입 어노테이션
   xCoord = (x :: Point2D -> Double) point2d
   ```
   
   **⚠️ 주의**: `x point2d :: Double`은 결과 타입만 명시하는 것이므로 여전히 모호합니다.
   컴파일러는 여전히 어떤 `x` 함수를 사용할지 모릅니다.
   
   b) **qualified import 사용**:
   ```haskell
   import qualified MyModule as M
   xCoord = M.x point2d
   ```
   
   c) **다른 필드 이름 사용** (권장):
   ```haskell
   data Point2D = Point2D {x2 :: Double, y2 :: Double}
   data Point3D = Point3D {x3 :: Double, y3 :: Double, z3 :: Double}
   ```

   **왜 파라미터 타입이 달라도 충돌이 발생하나?**
   
   Haskell의 타입 시스템에서는 **함수 이름이 같으면 같은 함수**로 간주됩니다. 
   타입이 다르더라도 함수 이름이 같으면 컴파일러가 어떤 함수를 사용해야 할지 모호해집니다.
   
   ```haskell
   -- 이 두 함수는 서로 다른 타입을 받지만, 같은 이름 'x'를 가짐
   x :: Point2D -> Double
   x :: Point3D -> Double
   
   -- 컴파일러 입장에서는 어떤 x를 사용할지 모호함
   somePoint :: Point2D
   somePoint = Point2D 1.0 2.0
   
   -- 이 코드에서 x는 Point2D의 x인지 Point3D의 x인지 모호함
   result = x somePoint  -- 컴파일 에러!
   ```
   
   **실제 예시로 확인해보기**:
   ```haskell
   -- 이 코드는 컴파일되지 않습니다!
   data Person = Person {name :: String, age :: Int}
   data Company = Company {name :: String, employees :: Int}
   
   -- name 함수가 두 타입 모두에 대해 생성되어 충돌
   -- Ambiguous occurrence 'name'
   ```

   **실용적인 해결책**:
   ```haskell
   -- 1. 접두사나 접미사 사용 (가장 권장)
   data Person = Person {personName :: String, personAge :: Int}
   data Company = Company {companyName :: String, companyEmployees :: Int}
   
   -- 2. 모듈로 분리
   module Person where
   data Person = Person {name :: String, age :: Int}
   
   module Company where  
   data Company = Company {name :: String, employees :: Int}
   
   -- 3. 패턴 매칭 사용 (타입 어노테이션보다 안전)
   person :: Person
   person = Person "John" 30
   
   -- 패턴 매칭으로 필드 추출
   personName = case person of Person {name = n} -> n
   
   **이 코드의 동작 원리**:
   ```haskell
   -- 1. person의 타입이 Person인지 확인
   -- 2. Person 생성자의 name 필드를 n이라는 변수에 바인딩
   -- 3. n을 반환
   ```

   **변수 바인딩이란?**
   
   변수 바인딩은 **값을 변수 이름에 연결하는 과정**입니다. Haskell에서는 패턴 매칭을 통해 값을 변수에 바인딩할 수 있습니다.
   
   ```haskell
   -- 기본적인 변수 바인딩
   x = 42                    -- x에 42를 바인딩
   name = "John"             -- name에 "John"을 바인딩
   
   -- 패턴 매칭을 통한 바인딩
   (a, b) = (1, 2)           -- a에 1, b에 2를 바인딩
   [x, y, z] = [1, 2, 3]     -- x에 1, y에 2, z에 3을 바인딩
   
   -- 레코드 패턴 매칭을 통한 바인딩
   Person {name = n, age = a} = Person "John" 30
   -- n에 "John", a에 30을 바인딩
   ```

   **레코드 필드 바인딩의 구체적인 예시**:
   ```haskell
   data Person = Person {name :: String, age :: Int}
   
   person :: Person
   person = Person "John" 30
   
   -- 방법 1: case 표현식에서 바인딩
   personName = case person of 
     Person {name = n} -> n  -- n에 "John"을 바인딩
   
   -- 방법 2: let 표현식에서 바인딩
   personName' = let Person {name = n} = person in n
   
   -- 방법 3: 함수 매개변수에서 바인딩
   getName (Person {name = n}) = n  -- n에 전달된 Person의 name 필드를 바인딩
   
   -- 방법 4: 여러 필드 동시 바인딩
   personInfo = let Person {name = n, age = a} = person in (n, a)
   -- n에 "John", a에 30을 동시에 바인딩
   ```

   **바인딩의 특징**:
   - **불변성**: 한 번 바인딩된 값은 변경할 수 없음
   - **지역성**: 바인딩된 변수는 해당 스코프 내에서만 유효
   - **타입 안전성**: 컴파일 타임에 타입 검사
   
   -- 단계별 분해:
   case person of                    -- person 값을 패턴 매칭
     Person {name = n} -> n          -- Person 타입이고 name 필드를 n에 바인딩
   
   -- 이는 다음과 동일합니다:
   personName = let Person {name = n} = person in n
   
   -- 또는 함수 정의로:
   personName = getName person
     where getName (Person {name = n}) = n
   ```

   **패턴 매칭의 장점**:
   ```haskell
   -- 1. 타입 안전성: 컴파일 타임에 타입 검사
   -- 2. 명확성: 어떤 필드를 추출하는지 명확
   -- 3. 확장성: 여러 필드를 한 번에 추출 가능
   
   -- 여러 필드 추출 예시:
   personInfo = case person of 
     Person {name = n, age = a} -> (n, a)  -- name과 age를 튜플로 반환
   
   -- 또는 더 간단하게:
   personInfo' = let Person {name = n, age = a} = person in (n, a)
   
   -- 실제 사용 예시:
   person :: Person
   person = Person "John" 30
   
   -- 결과: personName = "John"
   personName = case person of Person {name = n} -> n
   
   -- 결과: personInfo = ("John", 30)
   personInfo = case person of Person {name = n, age = a} -> (n, a)
   ```

   **왜 이 방법이 좋은가?**:
   - **타입 충돌 없음**: `name` 함수를 직접 호출하지 않으므로 충돌 문제가 없음
   - **명확한 의도**: 어떤 타입의 어떤 필드를 추출하는지 명확
   - **안전성**: 컴파일 타임에 타입 검사가 이루어짐
   
   -- 4. 함수 타입 어노테이션 (복잡하지만 가능)
   personName' = (name :: Person -> String) person
   
   -- 5. where 절에서 타입 명시
   personName'' = getName person
     where getName :: Person -> String
           getName (Person {name = n}) = n
   ```

   **핵심 포인트**: 
   - Haskell에서는 **함수 이름이 고유해야** 하므로, 레코드 필드 이름을 설계할 때 충돌을 피하는 것이 중요합니다!
   - `x point2d :: Double` 같은 결과 타입 어노테이션은 **모호함을 해결하지 못합니다**
   - 가장 실용적인 해결책은 **접두사/접미사 사용** 또는 **패턴 매칭**입니다
   - 함수 타입 어노테이션 `(x :: Point2D -> Double)`은 가능하지만 복잡합니다

2. **명확한 필드 의미**: 필드 이름으로 데이터의 의미를 명확히 표현할 수 있습니다

3. **필드 업데이트**: 레코드 업데이트 문법을 사용할 수 있습니다
   ```haskell
   v2 = v1 {x = 0}  -- v1의 x 필드만 0으로 변경
   ```

### 레코드형 vs 일반 타입 비교

```haskell
-- 일반적인 방식
data Vector2D_Old = MakeVector Double Double

-- 레코드형 방식
data Vector2D_New = MakeVector {x :: Double, y :: Double}

-- 사용법 비교
oldVector = MakeVector 1.0 2.0
newVector = MakeVector {x = 1.0, y = 2.0}

-- 접근 방법
-- oldVector: 패턴 매칭 필요
getX_Old (MakeVector x _) = x

-- newVector: 자동 생성된 접근자 사용
getX_New = x  -- x :: Vector2D_New -> Double
```

### 더 복잡한 레코드 예시

```haskell
data Person = Person 
    { name :: String
    , age :: Int
    , email :: String
    , address :: String
    } deriving (Show, Eq)

-- 생성
john :: Person
john = Person {name = "John", age = 30, email = "john@example.com", address = "Seoul"}

-- 필드 접근
johnName = name john  -- "John"
johnAge = age john    -- 30

-- 필드 업데이트
johnOlder = john {age = 31}  -- 나이만 31로 변경
```

## 다른 예시들

### Maybe 타입

```haskell
data Maybe a = Nothing | Just a
```

### Either 타입

```haskell
data Either a b = Left a | Right b
```

### 리스트 타입

```haskell
data List a = Nil | Cons a (List a)
```

## 장점

1. **타입 안전성**: 컴파일 타임에 타입 오류를 잡을 수 있습니다
2. **패턴 매칭**: 데이터 구조를 안전하게 분해할 수 있습니다
3. **재귀적 정의**: 자기 자신을 포함하는 타입을 정의할 수 있습니다
4. **제네릭**: 타입 변수를 사용해 다양한 타입에 대해 재사용 가능합니다
5. **레코드형의 추가 장점**:
   - **자동 접근자**: 필드 접근 함수가 자동으로 생성됩니다
   - **명확한 의미**: 필드 이름으로 데이터의 의미를 명확히 표현합니다
   - **편리한 업데이트**: 레코드 업데이트 문법으로 부분 수정이 가능합니다
   - **가독성**: 코드가 더 읽기 쉽고 이해하기 쉽습니다

## 요약

사용자 정의 타입은 Haskell의 핵심 기능 중 하나로, 복잡한 데이터 구조를 타입 안전하게 표현할 수 있게 해줍니다. 

- **기본 타입**: 타입 생성자와 데이터 생성자를 구분하고, 패턴 매칭을 통해 안전하게 데이터를 처리할 수 있습니다.
- **레코드형 타입**: 필드에 이름을 부여하여 더 명확하고 접근하기 쉬운 데이터 구조를 만들 수 있습니다. 자동 생성되는 접근자와 업데이트 문법으로 편리하게 사용할 수 있습니다.

두 방식 모두 타입 안전성을 보장하면서도 각각의 장점을 가지고 있어, 상황에 맞게 선택하여 사용할 수 있습니다.
