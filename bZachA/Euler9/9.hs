-- A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
--
-- a^2 + b^2 = c^2
-- For example, 32 + 42 = 9 + 16 = 25 = 52.
--
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.


-- well step one is finding the triplet a + b + c = 1000. that also satisfies a^2 + b^2 = c^2
-- naively we can start with a guess like, 220 + 230 + 350 = 1000
-- then calculate the squares and see if it works out
-- when it doesn't, we need to perturbate a,b,c in various permutations (either lower or higher depending on which was great, a^2 +b^2 or c^2)
-- that satisfy the arithmetic restraint of summing to 1000 as well as a < b < c as and check if the squares equal out again until
-- eventually they do, then return product
checkConstraint :: Int -> Int -> Int -> Bool
checkConstraint a b c = a^2 + b^2 == c^2

-- PROBLEM, this doesn't check the entire solution space
perturbateBasedOnGuessResult :: (Int,Int,Int) -> (Int,Int,Int)
perturbateBasedOnGuessResult (a,b,c) =
  if a^2 + b^2 > c^2 then
    (a, b, c + 1)
  else
    (a + 1, b + 1, c)


main :: IO ()
main = do
  -- let inputString = [r|7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450|]
  putStrLn "hello"
  -- let listOfProducts = calculateProductsOf13sGivenString $ map digitToInt inputString
  -- let maxProduct = maximum listOfProducts
  -- putStrLn "value of product"
  -- print maxProduct
  -- let numbersUpToCHOSENONE = takeWhile (/= maxProduct) listOfProducts
  -- putStrLn "13 adjacent digits:"
  -- print $ take 13 $ drop (length numbersUpToCHOSENONE) inputString
  -- print $ sieveOfErastothenesTo10001 2 []
