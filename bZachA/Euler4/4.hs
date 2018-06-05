-- A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import Control.Monad.State
import Data.Int

-- naive way: multiply every relevant combination of numbers together, then check if the product is a palindrome

-- what do we know about the products from the numbers being multiplied together?
-- we know if one or more of them is even, then the product is even

-- we know that last digit is function of last digit of both numbers product
-- but every subsequent digit depends more and more on previous digits
-- because there's more possible "carrying" happening

-- or we could search the other way: 
-- we could lazily generate all the palindromes starting from
-- 999,999
-- so,
-- 999999, 998899, 989989, 899998, 997799, .... 888888 ...
-- and just pick the first one that we could prove was the product of
-- two three digit numbers - the tricky question is how to test for that
-- naively generate the prime factors and find if there's some permutation of
-- them between two sets that results in two three digit numbers when multiplied together

-- that would probably be slower than just multiplying two three digit numbers together and checking if they result in a palindrome starting from the largest three digit numbers; or at least it wouldn't scale as well to really high numbers


multiplyAndCheckForPalindrome :: Int -> Int -> Bool
multiplyAndCheckForPalindrome inputNumber1 inputNumber2 =
  let stringRepresentation = show $ inputNumber1 * inputNumber2 in
  stringRepresentation == reverse stringRepresentation

-- 999 * 999, 998 * 999, 998 * 998, 997 * 999, 997 * 998, 997 * 997
generatePairsOfThreeDigitNumbersOrderedByProductSize :: Int -> Int -> [(Int, Int)]
generatePairsOfThreeDigitNumbersOrderedByProductSize onesPlaceCounter tensPlaceCounter hundredsPlaceCounter =
  -- let onesPlaceCounter = 9 in
  -- let tensPlaceCounter = 9 in
  -- let hundredsPlaceCounter = 9 in
  if onesPlaceCounter == 0 then
    if tensPlaceCounter == 0 then
      if hundredsPlaceCounter == 0 then
        [000]
      else
        [hundredsPlaceCounter tensPlaceCounter onesPlaceCounter] ++ generatePairsOfThreeDigitNumbersOrderedByProductSize 9 9 (hundredsPlaceCounter - 1)
      [hundredsPlaceCounter tensPlaceCounter onesPlaceCounter]  ++ generatePairsOfThreeDigitNumbersOrderedByProductSize 9 9 (hundredsPlaceCounter)

main :: IO ()
main = do
  -- putStrLn "enter inputNumber"
  theAnswer <- pure $ takeWhile multiplyAndCheckForPalindrome (generatePairsOfThreeDigitNumbersOrderedByProductSize 9 9 9)
  putStrLn "Da largest palindrome product of two three digit numbers is: "
  print $ theAnswer
