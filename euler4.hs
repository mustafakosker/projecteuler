main = do
         numTestCase <- getLine
         runTestCase $ read numTestCase

runTestCase :: Integer -> IO ()
runTestCase n
 | n == 0 = return ()
 | otherwise = do
                 number <- getLine
                 print $ findLargestPalindrome $ read number
                 runTestCase (n-1)

isFactorWithThreeDigits n = not $ null [(x, y) | x <- [100..999], let y = n `div` x,  n `mod` x == 0 && n `mod` y == 0 && y >= 100 && y <= 999]

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome (x:xs) = (x == (last xs)) && (isPalindrome (init xs))

findLargestPalindrome n = head [x | x <- [n, (n-1)..], isPalindrome (show x) && isFactorWithThreeDigits x]
