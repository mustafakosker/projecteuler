main = do
         testCaseNum <- getLine
         runTestCase $ read testCaseNum
         return ()

runTestCase :: Integer -> IO ()
runTestCase n
 | n <= 0 = return ()
 | otherwise = do number <- getLine
                  print $ calculateSum $ read number
                  runTestCase (n-1)

calculateSum :: Integer -> Integer
calculateSum n = calculateSumDiv n 3 + calculateSumDiv n 5 - calculateSumDiv n 15

calculateSumDiv :: Integer -> Integer -> Integer
calculateSumDiv n k = (m * (m+1) `div` 2) * k
 where m = (n-1) `div` k
