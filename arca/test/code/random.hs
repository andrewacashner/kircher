import System.Random
import Data.List

-- vperm :: State StdGen Int
-- vperm = state $ randomR (0, 9) 
-- 
-- main = do
--     g <- newStdGen
--     let n = evalState vperm g
--     print n

randomList :: (Int, Int) -> IO [Int]
randomList interval = 
    newStdGen >>= return . unfoldr (Just . randomR interval)

main = do
    vperms <- randomList (0, 9)
    rperms <- randomList (0, 4)
    putStrLn $ show $ take 10 (zip vperms rperms)
    putStrLn $ show $ map merge (take 10 (zip vperms rperms))

merge :: (Int, Int) -> Int
merge (x, y) = x + y

