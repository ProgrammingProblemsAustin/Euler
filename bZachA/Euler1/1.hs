-- If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
-- Find the sum of all the multiples of 3 or 5 below 1000.
main :: IO ()
main = do
  let theAnswer = foldl (\base numToTest ->
                            if numToTest `rem` 5 == 0 || numToTest `rem` 3 == 0 then
                              base + numToTest
                            else
                              base
                            ) 0 [1..999]
  putStrLn $ "The answer is " ++ show theAnswer

fastSpecificVersion :: Int
fastSpecificVersion =
  let
    mult5 = [j * 5 | j <- [1..199]]
    mult3 = [i * 3 | i <- [1..333]]
  in
    sum (mult5 ++ mult3)


-- foldl (\base numToTest ->
--                             if numToTest `rem` 5 == 0 || numToTest `rem` 3 == 0 then
--                               base + numToTest
--                             else
--                               base
--                             ) 0 [1..333]
