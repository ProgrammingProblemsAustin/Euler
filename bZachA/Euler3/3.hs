-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?
import Control.Monad.State
import Data.Int


--  def largest_prime_factor(number)
--    i = 2
--    while number > 1
--      if number % i == 0
--        number /= i;
--        i -= 1
--      end
--      i += 1
--    end
--    return i
--  end

stupidlySimpleLargestPrimeFactor :: Int -> Int -> Int
stupidlySimpleLargestPrimeFactor inputNumber counterStartsAt2 =
  if inputNumber > 1 then
    if inputNumber `rem` counterStartsAt2 == 0 then
      stupidlySimpleLargestPrimeFactor (inputNumber `div` counterStartsAt2) counterStartsAt2
    else
      stupidlySimpleLargestPrimeFactor inputNumber (counterStartsAt2 + 1)
  else
      counterStartsAt2

main :: IO ()
main = do
  putStrLn "enter inputNumber"
  inputNumberInput <- getLine
  let inputNumber = read inputNumberInput
  putStrLn "Da largest prime factor is: "
  print $ stupidlySimpleLargestPrimeFactor inputNumber 2






-- WELCOME TO THE GRAVEYARD OF BAD IDEAS!

-- -- MEMORY BRUTE FORCE way (doesn't work): generate all primes less than floor(number / 2) then divide by each of them until one is divisible (we know 2 isn't a candidate)
--
-- -- maxExtentPrimeSieveList `div` 2
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
--
-- findLargestDivisiblePrimeFromListOfPrimes :: [Int] -> Int -> Int
-- findLargestDivisiblePrimeFromListOfPrimes listOfPrimes numberToTestForDivisibility =
--   if null listOfPrimes then
--     -1 -- -1 means it's prime...
--   else
--   case numberToTestForDivisibility `rem` head listOfPrimes of
--     0 -> head listOfPrimes
--     _ -> findLargestDivisiblePrimeFromListOfPrimes (tail listOfPrimes) numberToTestForDivisibility
--
-- main :: IO ()
-- main = do
--   putStrLn "enter inputNumber"
--   inputNumberInput <- getLine
--
--   let inputNumber = read inputNumberInput ::Int
--   let listOfPrimesToTest = generatListOfPrimesLessThan inputNumber
--   -- putStrLn $ "listOfPrimesToTEst" ++ show listOfPrimesToTest
--   let theAnswer = findLargestDivisiblePrimeFromListOfPrimes listOfPrimesToTest inputNumber
--
--   -- let listOfFibs = takeWhile (>= upperBound) [ fib  | i <- [1..]]
--   -- foldl'
--
--   -- let theAnswer = sieveOfFactoring indexOfPrimeList listOfPrimes maxExtentOfPrimeSieveList listOfFactors inputNumber
--   putStrLn $ "the larget prime factor of" ++ show inputNumber  ++ " is " ++ show theAnswer






-- well let's start off the dumb way
-- sieve of erastophenes or something like that
-- take remainder with various primes, and cross off all their multiples
-- in practice we'll just build a big list and check if each consec

-- potential optimization: we know the size of the number so we can discount all primes under a certain size (definitely under floor(600851475143 / 2))

-- unless we "allocate memory" for a "list of candidates" beforehand, a prime sieve will not speed things up at all. but it will (really?) speed things up if we do.

-- while finding primes, we also need to find which primes are prime factors, of the target number and it's various "reductions" (so in essence we are having to go backwards and find the smallest prime factor first)


-- going to try and make this recursive instead of using state monad

-- sieveOfFactoring :: Int -> Int -> Int
-- sieveOfFactoring inputNumber maxExtentPrimeSieveList = flip evalState (3, [2..maxExtentPrimeSieveList], []) $ do
--   _ <- forM [0..(maxExtentPrimeSieveList/2)] $ \_ -> do
--     (numberToTest, testedPrimes, inputPrimeFactors) <- get
--     if inputNumber `rem` numberToTest == 0 then
--       if inputNumber == numberToTest * (product testedPrimes) then
--           put (numberToTest + 1, [], numberToTest:testedPrimes)
--         else
--           put (numberToTest + 1, [], numberToTest:testedPrimes)
--       else
--         put (numberToTest + 1, [], numberToTest:testedPrimes)
--   (_,_,c) <- get
--   return c

-- sieveOfFactoring 0 [] 3 [1] 600851475143
-- indexOfPrimeList should always start at 0, list ofPrimes as [], maxExtentPrimeSieveList > 2, listOfFactors [1]
-- foo:: Int -> Int -> Int
-- foo z x = if (z < 100)
--              then z * foo (z+(x*z)) z
--              else z

-- foo :: Int -> Int -> (IO (), Int)
-- foo z x = if z < 100 then (print z >> io, z * rec) else (return (), z) where
--     (io, rec) = foo (z+x*z) z
-- For example, you could print the recursive calls by setting

-- main = fst $ foo 13 7
-- or you could just print the answer by setting

-- main = print . snd $ foo 13 7
-- or half a dozen other things. Of course, the IO () type is a bit hard to inspect; you might consider writing something like this instead:

-- foo' :: Int -> Int -> Writer [Int] Int
-- foo' z x = if z < 100
--     then tell [z] >> fmap (z*) (foo' (z+x*z) z)
--     else return z
-- main = print . snd . runWriter $ foo' 13 7 -- to print a list of the calling values
-- main = print . fst . runWriter $ foo' 13 7 -- to print the result

-- -- TODO this should be approximately how to implement a writer monad - only problem is termination condition actually needs to cause termination

-- sieveOfFactoring :: Int -> [Int] -> Int -> [Int] -> Int -> Writer [Int, [Int], Int, [Int]] Int
-- sieveOfFactoring indexOfPrimeList listOfPrimes maxExtentPrimeSieveList listOfFactors inputNumber =
--   -- branch prediction bless us with your power, save us from our sins
--   if null listOfPrimes then
--       let newListOfPrimes = [2.. maxExtentPrimeSieveList] in
--       if inputNumber `rem` newListOfPrimes !! indexOfPrimeList == 0 then
--         if inputNumber == newListOfPrimes !! indexOfPrimeList * product listOfFactors then
--             return $ head $ tail listOfFactors
--         else
--             tell [indexOfPrimeList listOfPrimes maxExtentPrimeSieveList listOfFactors] >> sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` newListOfPrimes !! indexOfPrimeList) /= 0) newListOfPrimes) maxExtentPrimeSieveList ((newListOfPrimes !! indexOfPrimeList):listOfFactors) inputNumber

-- sieveOfFactoring :: Int -> [Int] -> Int -> [Int] -> Int -> Int
-- sieveOfFactoring indexOfPrimeList listOfPrimes maxExtentPrimeSieveList listOfFactors inputNumber
--   -- branch prediction bless us with your power, save us from our sins
--   | null listOfPrimes =
--       let newListOfPrimes = [2.. maxExtentPrimeSieveList] in
--       if inputNumber `rem` newListOfPrimes !! indexOfPrimeList == 0 then
--         if inputNumber == newListOfPrimes !! indexOfPrimeList * product listOfFactors then
--             head $ tail listOfFactors
--         else
--             sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` newListOfPrimes !! indexOfPrimeList) /= 0) newListOfPrimes) maxExtentPrimeSieveList ((newListOfPrimes !! indexOfPrimeList):listOfFactors) inputNumber
--       else
--         sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` newListOfPrimes !! indexOfPrimeList) /= 0) newListOfPrimes) maxExtentPrimeSieveList listOfFactors inputNumber
--   | inputNumber `rem` listOfPrimes !! indexOfPrimeList == 0 =
--       if inputNumber == listOfPrimes !! indexOfPrimeList * product listOfFactors then
--           head $ tail listOfFactors
--       else
--           sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` listOfPrimes !! indexOfPrimeList) /= 0) listOfPrimes) maxExtentPrimeSieveList (listOfPrimes !! indexOfPrimeList:listOfFactors) inputNumber
--   | otherwise =
--         sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` listOfPrimes !! indexOfPrimeList) /= 0) listOfPrimes) maxExtentPrimeSieveList listOfFactors inputNumber
-- sieveOfFactoring inputNumber maxExtentPrimeSieveList = flip evalState (3, [2..maxExtentPrimeSieveList], []) $ do
--   _ <- forM [0..(maxExtentPrimeSieveList/2)] $ \_ -> do
--     (numberToTest, testedPrimes, inputPrimeFactors) <- get
--     if inputNumber `rem` numberToTest == 0 then
--       if inputNumber == numberToTest * (product testedPrimes) then
--           put (numberToTest + 1, [], numberToTest:testedPrimes)
--         else
--           put (numberToTest + 1, [], numberToTest:testedPrimes)
--       else
--         put (numberToTest + 1, [], numberToTest:testedPrimes)
--   (_,_,c) <- get
--   return c
