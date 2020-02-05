-- regroup :: [[a, a, a, a], [a, a, a, a]] -> [[a, a], [a, a], [a, a], [a, a]] -- 
-- regroup :: [[a, a, a, a]] -> [[a][a][a][a]]

firsts :: [[a]] -> [a]
firsts [] = []
firsts [(x:xs)] = [x]
firsts ((x:xs):xss) = x : firsts xss

seconds :: [[a]] -> [a]
seconds [] = []
seconds [(x:xs)] = [head xs]
seconds ((x:xs):xss) = head xs : seconds xss

pivot :: [[a]] -> [[a]] -> [[a]]
pivot [] _ = []
pivot _ [] = [] 
pivot x y = (firsts x ++ firsts y) : pivot (tail x) (tail y)
