-- regroup :: [[a, a, a, a], [a, a, a, a]] -> [[a, a], [a, a], [a, a], [a, a]]
-- regroup :: [[a, a, a, a]] -> [[a][a][a][a]]
regroup [] = []
regroup [(a:as), (b:bs), (c:cs), (d:ds)] = [a, b, c, d] : regroup [as, bs, cs, ds]


