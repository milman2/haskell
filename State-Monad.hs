import Control.Monad.State
-- 기본 제공 함수 사용
-- get :: State ss
-- put :: s -> State s ()
-- modify :: (s -> s) -> State s ()
-- gets :: (s -> a) -> State s a

increment :: State Int String
increment = do -- State 모나드의 값 정의: 아직 실행되지 않는 계산
    n <- get
    put (n+1)
    return ("Increaed to " ++ show (n + 1))

-- runState : State 모나드의 값을 실행하고 결과를 반환
-- runState increment 10

program :: State Int String
program = do
    increment -- State Monad에서는 상태를 자동으로 전달함.
    increment
    n <- get
    return ("Final state: " ++ show n)

-- runState program 0
