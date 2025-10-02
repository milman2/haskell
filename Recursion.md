# recursion
```hs
-- Function calls itself
-- Problem: negative or large integer
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

main = do
    print(factorial (5))
```