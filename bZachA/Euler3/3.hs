-- The prime factors of 13195 are 5, 7, 13 and 29.

-- What is the largest prime factor of the number 600851475143 ?
import Control.Monad.State
import Data.Int

-- well let's start off the dumb way
-- sieve of erastophenes or something like that
-- take remainder with various primes, and cross off all their multiples
-- in practice we'll just build a big list and check if each consec

-- potential optimization: we know the size of the number so we can discount all primes under a certain size (definitely under floor(600851475143 / 2))

-- unless we "allocate memory" for a "list of candidates" beforehand, a prime sieve will not speed things up at all. but it will (really?) speed things up if we do.

-- while finding primes, we also need to find which primes are prime factors, of the target number and it's various "reductions" (so in essence we are having to go backwards and find the smallest prime factor first)


-- also need a way to test if all our prime factors

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

sieveOfFactoring :: Int -> Int -> [Int] -> Int
sieveOfFactoring inputNumber maxExtentPrimeSieveList initialListOfPrimes =
  if inputNumber `rem` numberToTest == 0 then
    if inputNumber == numberToTest * (product testedPrimes) then
      else
    sieveOfFactoring
  else
  where numberToTest = inputNumber + 1


  -- flip evalState (3, [2..maxExtentPrimeSieveList], []) $ do
  -- _ <- forM [0..(maxExtentPrimeSieveList/2)] $ \_ -> do
  --   (numberToTest, testedPrimes, inputPrimeFactors) <- get
  --   if inputNumber `rem` numberToTest == 0 then
  --     if inputNumber == numberToTest * (product testedPrimes) then
  --         put (numberToTest + 1, [], numberToTest:testedPrimes)
  --       else
  --         put (numberToTest + 1, [], numberToTest:testedPrimes)
  --     else
  --       put (numberToTest + 1, [], numberToTest:testedPrimes)
  -- (_,_,c) <- get
  -- return c


fib :: Int64 -> Int64
fib n = flip evalState (0,1,0) $ do
  _ <- forM [0..(n-1)] $ \_ -> do
    (a,b,c) <- get
    if a+b > 4000000 then
      put (a,b,c)
      else
      if even (a+b) then
        put (b,a+b, c+a+b)
        else
        put (b, a+b, c)
  (_,_,c) <- get
  return c


main :: IO ()
main = do
  upperBoundInput <- getLine
  let upperBound = read upperBoundInput ::Int64
  -- let listOfFibs = takeWhile (>= upperBound) [ fib  | i <- [1..]]
  -- foldl'

  -- let theAnswer = foldl (\base numToTest ->
  --                           if numToTest `rem` 5 == 0 || numToTest `rem` 3 == 0 then
  --                             base + numToTest
  --                           else
  --                             base
  --                           ) 0 [1..999]
  putStrLn $ "upper bound is " ++ show upperBound
  putStrLn $ "The answer is " ++ show (fib upperBound)
