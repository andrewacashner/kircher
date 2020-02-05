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

pivot :: [[a]] -> [[a]]
pivot [] = []
pivot [x:xs] = [x:xs]
pivot ((x:xs):xss) = (x:xs) : pivot xss

