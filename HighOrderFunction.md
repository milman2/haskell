# high order function
```hs
-- map
-- filter
-- foldl, foldr
-- head, tail
-- zipWith, zipWithN
-- concatMap
-- scanl, scanr
-- replicate
-- sequence

-- curry
-- compose
-- pipe

-- classify
-- classifyr
-- scan

let increment x = x + 1
map increment [1, 2, 3]

let isEven n = n `mod` 2 == 0
filter isEven [1, 2, 3]

foldl (+) 0 [1, 2, 3, 4, 5]
foldr (*) 1 [1, 2, 3, 4, 5]

import Data.List (sort)
sort [12, 535, 123, 448823, 2211]

zipWith (+) [1, 2, 3] [4, 5, 6, 7]

takeWhile (<3) [1, 2, 3, 4, 5]
dropWhile (<3) [1, 2, 3, 4, 5]

all even [1, 2, 3, 4]
any even [1, 2, 3, 4]
```