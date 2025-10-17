data Q = Q Int Int

instance Show Q where
    show (Q n d) = concat [show n, "/", show d]

simpQ :: Q -> Q
simpQ (Q n d) = Q (n `div` c) (d `div` c)
    where c = gcd n d

instance Eq Q where
    -- (Q x y) == (Q x' y') = x == x' && y == y'
    r1 == r2 = (n1 == n2) && (d1 == d2)
        where (Q n1 d1) = simpQ r1
              (Q n2 d2) = simpQ r2
-- Q 10 5 == Q 2 1
-- simpQ $ Q 10 5

addQ :: Q -> Q -> Q
addQ (Q n1 d1) (Q n2 d2) = simpQ $ (Q (n1' * n2') `div` m)
    where m = lcm d1 d2
          n1' = n1 * (m `div` d1)
          n2' = n2 * (m `div` d2)

mulQ :: Q -> Q -> Q
mulQ (Q n1 d1) (Q n2 d2) = simpQ $ Q (n1 * n2) (d1 * d2)

instance Num Q where
    (+)            = addQ
    negate (Q n d) = Q (-n) d
    (*)            = mulQ
    abs (Q n d)    = Q (abs n) (abs d)
    signum (Q n d) = Q (signum n * signum d) 1
    fromInteger n  = Q (fromInteger n) 1


-- addQ 
-- abs $ (Q (-1) 2)
-- Q (-1) 10 * Q 1 2
-- Q (-1) 10 + Q 1 2
