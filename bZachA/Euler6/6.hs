
main :: IO ()
main = do
  putStrLn "compute the sum of the squares of the first 100 natural numbers"
  print $ sum $ map (^2) [1..100]
  -- theAnswer <- pure $ sort $ map multiply2Tuple $ filter multiplyAndCheckForPalindrome (generatePairsOfThreeDigitNumbersOrderedByProductSize 999 [999])
  -- putStrLn "Da largest palindrome product of two three digit numbers is the last item in this list + 1: "
  -- print $ theAnswer
