adjust :: [Int] -> [Int]
adjust [] = []
adjust (a:[]) = [a]
adjust (a:b:bs) = (a:(adjust (b2:bs)))
    where b2 | a - b >=  10 = b + 10
             | a - b <= -10 = b - 10
             | otherwise    = b

