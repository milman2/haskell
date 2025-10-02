# where
```hs
-- Hwere cluase is an awesome construct in Haskell
popDensity :: (Float, Float) -> Float
popDensity (population, area) = density where density = population / area

main = do
    print(popDensity (1100000000.25, 850.27))
```