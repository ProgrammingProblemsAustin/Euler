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

-- indexOfPrimeList should always start at 0, list ofPrimes as [], maxExtentPrimeSieveList > 2, listOfFactors [1]
sieveOfFactoring :: Int -> [Int] -> Int -> [Int] -> Int -> Int
sieveOfFactoring indexOfPrimeList listOfPrimes maxExtentPrimeSieveList listOfFactors inputNumber =
  -- branch prediction bless us with your power, save us from our sins
  if null listOfPrimes then
      let newListOfPrimes = [2.. maxExtentPrimeSieveList] in
      if inputNumber `rem` newListOfPrimes !! indexOfPrimeList == 0 then
        if inputNumber == newListOfPrimes !! indexOfPrimeList * product listOfFactors then
            head $ tail listOfFactors
          else
            sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` newListOfPrimes !! indexOfPrimeList) /= 0) newListOfPrimes) maxExtentPrimeSieveList ((newListOfPrimes !! indexOfPrimeList):listOfFactors) inputNumber
      else
        sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` newListOfPrimes !! indexOfPrimeList) /= 0) newListOfPrimes) maxExtentPrimeSieveList listOfFactors inputNumber
    else
      if inputNumber `rem` listOfPrimes !! indexOfPrimeList == 0 then
        if inputNumber == listOfPrimes !! indexOfPrimeList * product listOfFactors then
            head $ tail listOfFactors
          else
            sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` listOfPrimes !! indexOfPrimeList) /= 0) listOfPrimes) maxExtentPrimeSieveList (listOfPrimes !! indexOfPrimeList:listOfFactors) inputNumber
      else
        sieveOfFactoring indexOfPrimeList (filter (\num -> (num `rem` listOfPrimes !! indexOfPrimeList) /= 0) listOfPrimes) maxExtentPrimeSieveList listOfFactors inputNumber

main :: IO ()
main = do
  putStrLn "enter indexOfPrimeList"
  indexOfPrimeListInput <- getLine
  putStrLn "enter listOfPrimes"
  listOfPrimesInput <- getLine
  putStrLn "enter maxExtentPrimeSieveList"
  maxExtentPrimeSieveListInput <- getLine
  putStrLn "enter listOfFactors"
  listOfFactorsInput <- getLine
  putStrLn "enter inputNumber"
  inputNumberInput <- getLine

  let indexOfPrimeList = read indexOfPrimeListInput ::Int
  let listOfPrimes = read listOfPrimesInput ::[Int]
  let maxExtentOfPrimeSieveList = read maxExtentPrimeSieveListInput ::Int
  let listOfFactors = read listOfFactorsInput ::[Int]
  let inputNumber = read inputNumberInput ::Int

  -- let listOfFibs = takeWhile (>= upperBound) [ fib  | i <- [1..]]
  -- foldl'

  let theAnswer = sieveOfFactoring indexOfPrimeList listOfPrimes maxExtentOfPrimeSieveList listOfFactors inputNumber
  putStrLn $ "the larget prime factor of" ++ show inputNumber  ++ " is " ++ show theAnswer
