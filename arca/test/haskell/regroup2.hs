regroup :: Foldable t => [t a] -> Int -> [[t a]]
regroup ls max = innerRegroup ls [] 
    where
        innerRegroup :: Foldable t => [t a] -> [t a] -> [[t a]]
        innerRegroup [] new = [reverse new]
        innerRegroup old new = 
            let next = (head old) : new in
            if (sum $ map length next) <= max 
                then innerRegroup (tail old) next 
                else (reverse new):(innerRegroup old [])


