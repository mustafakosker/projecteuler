main = do
         numTestCase <- getLine
         runTestCase $ read numTestCase

runTestCase :: Integer -> IO ()
runTestCase numTestCase
 | numTestCase <= 0 = return ()
 | otherwise = do
                  number <- getLine
                  print $ findPrime $ read number
                  runTestCase (numTestCase-1)

findPrime :: Integer -> Integer
findPrime n = findLargestFactor n 2 2

findLargestFactor :: Integer -> Integer -> Integer -> Integer
findLargestFactor n m i
 | prime n = max m n
 | (n `mod` i == 0) = findLargestFactor (n `div` i) (max m i) i
 | otherwise = findLargestFactor n m (findFactor n)

findFactor :: Integer -> Integer
findFactor n = head $ filter (\x -> (n `mod` x == 0) && (prime x)) [m,(m-1)..2]
 where m = isqrt n

prime :: Integer -> Bool
prime n =  null [x | x <- [2..m], (n `mod` x == 0)]
 where m = isqrt n

isqrt :: Integer -> Integer
isqrt = floor . sqrt . fromInteger
