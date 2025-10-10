# Zipper
- 어떤 자료구조를 **현재 위치**와 **나머지 구조**로 나누어 표현하는 기법
- **부분적으로 포커스를 맞추고**, 그 위치에서 **삽입, 삭제, 이동**을 쉽게

# 언제 유용?
| 상황              | Zipper의 장점                                  |
|-----------------|-----------------------------------------------|
| 리스트에서 커서 이동 | 왼쪽, 현재, 오른쪽으로 나눠서 빠르게 이동               |
| 트리에서 노드 수정   | 부모 정보와 경로를 기억하면서 수정 가능                  |
| 에디터 구현         | 텍스트 커서 위치 기반 편집에 적합                       |
| Undo/Redo       | 이전 상태를 저장하고 되돌리기 쉽게 구현 가능              |

# 예시
```hs
import Control.Monad (join)

-- Define the list zipper type
-- (왼쪽 부분, 오른쪽 부분)
-- 오른쪽 리스트의 첫 번째 요소가 현재 포커스
-- 왼쪽 리스트는 포커스를 지나온 값들을 담고 있으며, 역순으로 저장
type ListZipper a = ([a],[a])

-- Function to view the current focus of the zipper
focus :: ListZipper a -> Maybe a
focus (_,[]) = Nothing
focus (_, x:_) = Just x

insert :: a -> ListZipper a -> ListZipper a
insert new (left, right) = (left, new : right)

delete :: ListZipper a -> Maybe (ListZipper a)
delete (left, [])     = Nothing
delete (left, _:rest) = Just (left, rest)

replace :: a -> ListZipper a -> Maybe (ListZipper a)
replace _ (left, [])     = Nothing
replace new (left, _:xs) = Just (left, new : xs)

-- Move the zipper to the right
goRight :: ListZipper a -> Maybe (ListZipper a)
goRight (left, x:right) = Just (x:left, right)
goRight (_,[]) = Nothing
-- Move the zipper to the left
goLeft :: ListZipper a -> Maybe (ListZipper a)
goLeft (x:left, right) = Just (left, x:right)
goLeft (_,[]) = Nothing

main :: IO ()
main = do
    let zipper = ([],[1,2,3,4,5,6])
    putStrLn "Original Focus:"
    print $ focus zipper

    putStrLn "After moving to Right:"
    -- print $ fmap focus (goRight zipper)
    -- print $ join $ fmap focus (goRight zipper)
    print $ goRight zipper >>= return . focus    

    putStrLn "After moving to Left:"
    print $ fmap focus (goRight zipper >>= goLeft)
```