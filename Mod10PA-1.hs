--
-- Mod10PA.hs -- Module 10 Haskell functions
-- David Thompson

-- Generate a list of all factors of n
factors :: Integral a => a -> [a]
factors n = [ i | i <- [1..n], n `mod` i == 0]

-- True iff n is prime
isPrime :: Integral a => a -> Bool
isPrime n = factors n == [1, n]
     
-- Generate a list of all prime factors of n
primeFactors :: Integral a => a -> [a]
primeFactors 1 = []
primeFactors n = [x | x <- [1..n], n `mod` x == 0 && isPrime x]

-- Generate a list of primes up to n
primesUpTo :: Integral a => a -> [a]
primesUpTo n = [x | x <- [2..n], isPrime x]

-- True iff n is a perfect number
-- A number n is perfect if the sum of its factors is 2*n
isPerfect :: Integral a => a -> Bool
isPerfect n 	
	| n < 0 = False 
	| n == 0 = False
	| otherwise = sum (factors n) == 2 * n

-- Generate a list of all perfect numbers up to n
perfectUpTo :: Integral a => a -> [a]
perfectUpTo n = [x | x <- [1..n], sum(init (factors x)) == x]


-- Generate the next prime greater than n
nextPrime :: Integral a => a -> a
nextPrime n
	| n < 2 = 2
	| otherwise	=  head $ filter isPrime[(n+1)..]

-- Generate the first n primes
generatePrimes :: Integral a => a -> [a]
generatePrimes n 
	| n < 1 = []
	| otherwise = []

-------------------------------------
-- Tests: Use this code as is to make
-- sure your PA is correct.

testFactors =

  (factors 1) == [1] &&
  (factors 2) == [1,2] &&
  (factors 3) == [1,3] &&
  (factors 4) == [1,2,4] &&
  (factors 5) == [1,5] &&
  (factors 24) == [1,2,3,4,6,8,12,24]

testIsPrime =
  not (isPrime (-5)) &&
  not (isPrime 0) &&
  not (isPrime 1) &&
      (isPrime 2) &&
      (isPrime 3) &&
  not (isPrime 4) &&
      (isPrime 5) &&
      (isPrime 17389)

testPrimeFactors =
  primeFactors (-3) == [] &&
  primeFactors 0 == [] &&
  primeFactors 1 == [] &&
  primeFactors 5 == [5] &&
  primeFactors 24 == [2,3] &&
  primeFactors 40320 == [2,3,5,7]

testPrimesUpTo = 
  primesUpTo (-2) == [] &&
  primesUpTo 2 == [2] &&
  primesUpTo 5 == [2,3,5] &&
  primesUpTo 8 == [2,3,5,7] &&
  primesUpTo 43 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43]

testIsPerfect =
  not (isPerfect (-3)) &&
  not (isPerfect 0) &&
  not (isPerfect 1) &&
  not (isPerfect 2) &&
       isPerfect 6 &&
       isPerfect 28 &&
  not (isPerfect 426)

testPerfectUpTo =
  perfectUpTo (-7) == [] &&
  perfectUpTo 27 == [6] &&
  perfectUpTo 28 == [6,28]
  
testNextPrime =
  nextPrime (-1) == 2 &&
  nextPrime 5 == 7 &&
  nextPrime 200 == 211

testGeneratePrimes =
  generatePrimes (-3) == [] &&
  generatePrimes 0 == [] &&
  generatePrimes 1 == [2] &&
  generatePrimes 15 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47]

test =
  testFactors &&
  testIsPrime &&
  testPrimeFactors &&
  testPrimesUpTo &&
  testIsPerfect &&
  testPerfectUpTo &&
  testNextPrime &&
  testGeneratePrimes

-----------------------------------------------------------------
-- The stuff below is for compiling and running (by typing 
-- runhaskell Mod10PA.hs, for example) your code to run all tests.
-- In ghci, execute test or main.
main =
  do 
    let result = "factors: " ++ (show testFactors) ++ "\n" ++
                 "isPrime: " ++ (show testIsPrime) ++ "\n" ++
                 "primeFactors: " ++ (show testPrimeFactors) ++ "\n" ++
                 "primesUpTo: " ++ (show testPrimesUpTo) ++ "\n" ++
                 "isPerfect: " ++ (show testIsPerfect) ++ "\n" ++
                 "perfectUpTo: " ++ (show testPerfectUpTo) ++ "\n" ++
                 "nextPrime: " ++ (show testNextPrime) ++ "\n" ++
                 "generatePrimes: " ++ (show testGeneratePrimes)
    putStrLn result

