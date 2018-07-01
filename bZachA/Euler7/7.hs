-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?



-- generatListOfPrimesLessThan :: Int -> [Int]
-- generatListOfPrimesLessThan maxExtent = flip evalState ([2]) $ do
--   _ <- forM [3..maxExtent] $ \numToTest -> do
--     (primes) <- get
--     let isPrime = foldl (\baseCase numberToTestWith -> if numToTest `rem` numberToTestWith == 0 then baseCase && False else baseCase && True) True primes
--     if isPrime then
--       put (numToTest:primes)
--       else
--       put (primes)
--   (c) <- get
--   return c

-- not the most efficient method in the world ... takes a while to run.
-- think it's a bit faster with foldr than foldl, maybe 30% (perhaps there is some gain from laziness, but since the largest primes are first in the last it still has to evaluate a lot of them anyway, could probbaly do better with a doubly linked list or something where 2 is evaluated first)
sieveOfErastothenesTo10001 :: Int -> [Int] -> Int
sieveOfErastothenesTo10001 inputNum primeList =
  if length primeList == 10001 then
    head primeList
  else
    -- let isPrime = foldl (\baseCase numberToTestWith -> baseCase && (inputNum `rem` numberToTestWith /= 0)) True primeList in
    let isPrime = foldr (\numberToTestWith baseCase -> baseCase && (inputNum `rem` numberToTestWith /= 0)) True primeList in
    if isPrime then
      sieveOfErastothenesTo10001 (inputNum + 1) (inputNum : primeList)
    else
      sieveOfErastothenesTo10001 (inputNum + 1) primeList


main :: IO ()
main = do
  print $ sieveOfErastothenesTo10001 2 []
  -- theAnswer <- pure $ sort $ map multiply2Tuple $ filter multiplyAndCheckForPalindrome (generatePairsOfThreeDigitNumbersOrderedByProductSize 999 [999])
  -- putStrLn "Da largest palindrome product of two three digit numbers is the last item in this list + 1: "
  -- print $ theAnswer
