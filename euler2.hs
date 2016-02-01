main = do
         testCaseNum <- getLine
         runTestCase $ read testCaseNum
         return ()

runTestCase :: Integer -> IO ()
runTestCase n
 | n <= 0 = return ()
 | otherwise = do number <- getLine
                  print $ fibonacciEvenSum $ read number
                  runTestCase (n-1)

fibonacciEvenSum :: Integer -> Integer
fibonacciEvenSum n = addAll [x | x <- fibonacci m, even x && x < n]
 where m = truncate (logBase 1.618 ((5 ** 0.5) * (realToFrac n)))

addAll = foldr (+) 0

fibonacci :: Integer -> [Integer]
fibonacci 1  = [1]
fibonacci 2  = (fibonacci 1) ++ [2]
fibonacci n  = [addAll $ filter even (init fib)] ++ [last fib] ++ (sumDrop fib)
 where fib = fibonacci (n-1)

sumDrop :: [Integer] -> [Integer]
sumDrop xs = [addAll $ drop (length xs - 2) xs]
