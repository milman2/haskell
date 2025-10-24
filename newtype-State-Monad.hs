import Data.Char

newtype State s a = State { runState :: s -> (a, s)}

-- State 모나드 인스턴스 정의
instance Functor (State s) where
    fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    State f <*> State g = State $ \s -> let (h, s') = f s; (a, s'') = g s' in (h a, s'')

instance Monad (State s) where
    return = pure
    State f >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

-- State 모나드 유틸리티 함수들
get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

-- 사용 예시들
increment :: State Int Int
increment = do
    n <- get
    put (n + 1)
    return n

-- 실전 예제 : 스택
type Stack = [Int]

push :: Int -> State Stack ()
push x = modify (x:)

pop :: State Stack Int
pop = State $ \(x:xs) -> (x, xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    push 5
    pop

-- runState stackManip [1,2,3]