module Comp.Num 
( divisible, tau, factorial, squares, square, sqrtn, sumOfN, sumOfSquares,
  sumOfCubes,
  fibonacci, fibonacci',
  binaryGCD, factors,
  isPrime, isPrime',
  primes, primes', doprimes, primeFactors, primeFactors', primeBefore, primeAfter,
  numOfFactors, numOfFactors',
  triangleNums, triangleNums',
  sumOfnCn, nCr, nCn, nPn, nPr, permutationsR, combinations, nthPermutation,
  collatzChain, pascalTriangle, gridPath, amicables,
  isPerfectNum, perfectNumbers, abundantNumbers, deficientNumbers,
  isTerminatingDecimal, isCoprime, coprimes, repetend,
  coinChanges,
  -- For testing purpose
  fibslice
)
where

import Data.List (reverse, group, delete, tails)
import Data.Bits (shift, testBit, (.|.))
import Random (RandomGen, randomR)

import Comp.List

-- Required for `doprimes`
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
 

-- Division check for n,
--   Given a list of numbers check whether anyof them properly divides `n`.
divisible divisors n = any (==0) $ map (rem n) divisors

-- Golden Ratio constant
tau = ( 1 + sqrt(5) ) / 2

-- Factorial of `n`
factorial n 
    | n == 0 = 1
    | otherwise = product [1..n]

-- Sequential list of square numbers
square n = n * n
squares = map square [1..]

-- Square root for Integer `n`
sqrtn :: (Integral a) => a -> Integer
sqrtn n = ceiling $ sqrt (fromIntegral n :: Float)

-- A square number on or before `n`
squareBefore n = square $ floor $ sqrt (fromIntegral n :: Float)

-- A square number on or after `n`
squareAfter n = square $ ceiling $ sqrt (fromIntegral n :: Float)

-- Sum of N natural numbers, their squares and cubes
sumOfN n       = n * (n+1) `quot` 2
sumOfSquares n = n * (n+1) * ((2*n) + 1) `quot` 6
sumOfCubes   n = (square n) * (square $ n+1) `quot` 4


-- Fibonacci
--
-- | F-8 F-7 F-6 F-5 F-4 F-3 F-2 F-1 F0  F1  F2  F3  F4  F5  F6  F7  F8
-- | -21 13  -8  5   -3  2   -1  1   0   1   1   2   3   5   8   13  21
fibonacci  = 0:1:(zipWith (+) fibonacci (tail fibonacci))
fibonacci' = zipWith (*) (map (\x -> (-1) ^ (x+1)) [1..]) (tail fibonacci)
fibslice from to
  | natural = take (to-from+1) (drop from fibonacci)
  | integer = let neg = take (abs from) fibonacci'
                  pos = take (to+1) fibonacci
              in (reverse neg) ++ pos
  | valid   = reverse (take (abs (from)) (drop (abs (to+1)) fibonacci'))
  where
    natural = from >= 0 && to >= 0
    integer = to >= 0
    valid   = from < to


-- | binaryGCD
--
--  The algorithm reduces the problem of finding the GCD by repeatedly applying
--  these identities:
--
--  1. gcd(0, v) = v, because everything divides zero, and v is the largest
--     number that divides v. Similarly,
--     gcd(u, 0) = u. gcd(0, 0) is not typically defined, but it is convenient 
--     to set gcd(0, 0) = 0.
--  2. If u and v are both even, then gcd(u, v) = 2·gcd(u/2, v/2), because 2 is
--     a common divisor.
--  3. If u is even and v is odd, then gcd(u, v) = gcd(u/2, v), because 2 is 
--     not a common divisor. Similarly, 
--     if u is odd and v is even, then gcd(u, v) = gcd(u, v/2).
--  4. If u and v are both odd, and u ≥ v, then gcd(u, v) = gcd((u − v)/2, v).
--     If both are odd and u < v, then gcd(u, v) = gcd((v − u)/2, u). These
--     are combinations of one step of the simple Euclidean algorithm, which
--     uses subtraction at each step, and an application of step 3 above. The
--     division by 2 results in an integer because the difference of two odd
--     numbers is even.
--  5. Repeat steps 2–4 until u = v, or (one more step) until u = 0. In either
--     case, the GCD is 2kv, where k is the number of common factors of 2 found
--     in step 2.
binaryGCD :: Integer -> Integer ->Integer
binaryGCD 0 v = v
binaryGCD u 0 = u
binaryGCD u v
  | not $ testBit (u .|. v) 0 = binaryGCD (div2 u) (div2 v) `shift` 1
  | not $ testBit u 0         = binaryGCD (div2 u) v
  | not $ testBit v 0         = binaryGCD u (div2 v)
  | u >= v = binaryGCD (div2 $ u-v) v
  | u <  v = binaryGCD (div2 $ v-u) u
  where
    div2 n = n `shift` (-1)


-- | Return natural factors of a number
factors 2 = [2,1]
factors n = let till = sqrtn n in n:1:go n [2..till]
  where
    go n []                = []
    go n (x:xs)        -- collect natural factors
      | r == 0 && q==x     = [q]
      | r == 0 && q==(x+1) = [q,x]
      | r == 0             = q:x:go n xs
      | otherwise          = go n xs
      where
        (q,r) = quotRem n x


-- | Check whether the given Integer is a prime number
isPrime :: Integer -> Bool
isPrime n
  | n > 1     = isprime n
  | otherwise = False
  where
    isprime n = foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes


-- | Check whether the given native Integer is a prime number
isPrime' :: Int -> Bool
isPrime' n
  | n > 1     = isprime n
  | otherwise = False
  where
    primes = doprimes $ fromIntegral $ sqrtn n
    isprime n = foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes
                     

-- | Performs a Miller Rabin Primality Test. According to the Wikipedia it's 
-- false positive with a probability of less than 25%. It's never false 
-- negative. Use it several times to increase confidence.
--isProbablyPrime :: RandomGen g => Integer -> g -> (Bool, g)
--isProbablyPrime n gen
--  | n < 2     = (False,gen')
--  | n == 2    = (True, gen')
--  | even n    = (False, gen')
--  | b0 == 1 || b0 == n' = (True, gen')
--  | otherwise = (iter (tail b), gen')
--  where
--    (a, gen') = randomR (2,n-2) gen
--    n'        = n-1
--    (k,m)     = find2km n'
--    b0        = powMod n a m
--    b         = take (fromIntegral k) $ iterate (squareMod n) b0
--    iter []   = False
--    iter (x:xs)
--      | x == 1 = False
--      | x == n' = True
--      | otherwise = iter xs
--    find2km :: Integral a => a -> (a,a)
--    find2km n = f 0 n
--      where 
--        f k m
--          | r == 1 = (k,m)
--          | otherwise = f (k+1) q
--          where (q,r) = quotRem m 2        

 

-- | Return prime factors for a number
primeFactors :: Integer -> [Integer]
primeFactors = (primefactors primes)

primefactors pps@(p:ps) n          -- Local declaration
  | p*p > n   = [n]
  | r == 0    = p: primefactors pps q
  | otherwise = primefactors ps n
  where 
    (q,r) = quotRem n p


-- | Return prime factors for a number, in native integer
primeFactors' :: Int -> [Int]
primeFactors' n = primefactors (primes n) n
  where 
    primes = doprimes . fromIntegral . sqrtn


-- | Generate an infinite list of prime numbers using sieve of eratosthenes,
-- | twice faster than the above algorithm
primes :: (Integral a) => [a]
primes = 2:3:lazyprimes
  where
    lazyprimes = [5,7..] `minusS` foldi fn [] [ [p*p, p*p+2*p..] | p <- tail primes]
    fn (x:xs) = (x:).(unionS xs)


-- | Generate an list of prime numbers (native integers) using sieve of 
--   eratosthenes
primes' :: [Int]
primes' = 2:3:lazyprimes
  where
    lazyprimes = [5,7..] `minusS` foldi fn [] [ [p*p, p*p+2*p..] | p <- tail primes']
    fn (x:xs) = (x:).(unionS xs)


-- | Generate list of prime numbers (native integers) using monadic algorithm
doprimes :: Int -> [Int]
doprimes top = 2:[ i*2+1 | (i,True) <- assocs $ sieveUA top]
  where 
    sieveUA :: Int -> UArray Int Bool
    sieveUA top = runSTUArray $ do
      let m = (top-1) `div` 2
          r = floor . sqrt $ fromIntegral top + 1
      sieve <- newArray (1,m) True          -- :: ST s (STUArray s Int Bool)
      forM_ [1..r `div` 2] $ \i -> do
        isPrime <- readArray sieve i
        when isPrime $ do                   -- ((2*i+1)^2-1)`div`2 == 2*i*(i+1)
          forM_ [2*i*(i+1), 2*i*(i+2)+1..m] $ \j -> do
            writeArray sieve j False
      return sieve


-- prime numbers before and after n
primeBefore n = head $ filter isPrime [n,n-1..1]
primeAfter n  = head $ filter isPrime [n..]


-- | Returns the number of divisors for a positive integer.
--   http://mathschallenge.net/index.php?section=faq&ref=number/number_of_divisors
numOfFactors = (toInteger . product . (map ((+1).length)) . group . primeFactors)
numOfFactors' = (product . (map ((+1).length)) . group . primeFactors')


-- | Sequential list of triangle Numbers
triangleNums = map sumOfN [1..] :: [Integer]
triangleNums' = map sumOfN [1..] :: [Int]


-- | Sequential list triangle Numbers that are also square numbers.
triangleSquares@(_:ts) = 0:1:zipWith (\x y -> (34*y)-x+2) triangleSquares ts


-- | Exponential list of triangle numbers 9^n, based on the following property
-- |   If T is a Triangular number than 9*T + 1 is also a Triangular number
triangleNums9 = 1:[ (9*t)+1 | t <- triangleNums9 ]


-- | Relationship between square numbers and triangle numbers
-- | (n*n) = (nth triangle number) + ( n-1 the triangle number )
triangleForSquare n = ((sumOfN n-1), (sumOfN n))


-- | Collaz sequence (Not yet prooved)
collatzChain n
  | n == 1    = [1]
  | otherwise = n:collatzChain (if r == 0 then q else (3*n)+1)
  where 
    (q,r) = n `divMod` 2


-- | Pascal's triangle
pascalTriangle = [1]:map ((1:).sumFold) pascalTriangle
  where
    sumFold [x]          = [x]
    sumFold (x:ls@(y:_)) = (x+y):sumFold ls
                                  

-- | Total number of possible shortest paths in a NxN grid. Also called
--   `city grid` (no-backtracking)
gridPath n = maximum $ head $ drop (2*n) pascalTriangle


-- | Permutation and combination
nPn n   = factorial n
nCn n   = 1
nPr n r = product [n,n-1..n-r+1]
nCr n r = (n `nPr` r) `quot` (factorial r)

sumOfnCn n = (2^n) - 1

-- List of permutations `r` for list of symbols `ls`
permutationsR 1 ls = map (:[]) ls
permutationsR r ls = concat $ map permute ls
  where permute l = map (l:) $ permutationsR (r-1) (delete l ls)

-- Nth lexicographic permutation for given list of sorted symbols `ls`
-- N starting from 0
nthPermutation nth 1 ls = [ls !! nth]
nthPermutation nth r ls = (ls!!d):nthPermutation m (r-1) (delete (ls!!d) ls)
  where (d,m) = nth `quotRem` nPr ((length ls)-1) (r-1)
        
-- List of combinations `r` for list of symbols `ls`
combinations 1 ls         = map (:[]) ls
combinations r lls@(l:ls) = concat $ zipWith combine lls $ init $ tails ls
  where combine l t = map (l:) $ combinations (r-1) t

-- Nth lexicographic permutation for given list of sorted symbols `ls`
-- N starting from 0
nthCombination nth 1 ls     = [ls!!nth]
nthCombination nth r (l:ls)
  | nth < x   = l:nthCombination nth (r-1) ls
  | otherwise = nthCombination (nth-x) r ls
  where x = nCr (length ls) (r-1)


-- Amicable numbers
isAmicable x y = if divSum x == y then divSum y == x else False
  where divSum n = (sum $ factors n) - n

amicables n = roll n (divSum n)
  where
    roll x y
      | x == 3          = []
      | x == y || y>=n  = roll next (divSum next)
      | x == (divSum y) = x:roll next (divSum next)
      | otherwise       = roll next (divSum next)
      where next = x-1
    divSum n = (sum $ factors n) - n


-- Perfect numbers, abundant numbers and deficient numbers 
isPerfectNum n = compare (divSum n) n
  where divSum n = (sum $ factors n) - n

perfectNumbers   = filter ((EQ==).isPerfectNum) [28..]
deficientNumbers = filter ((LT==).isPerfectNum) [2..]
abundantNumbers  = filter ((GT==).isPerfectNum) [2..]


-- Coprimes
isCoprime n m = (gcd n m) == 1


-- The number of integers coprime to a positive integer n, between 1 and n, is
-- given by Euler's totient function (or Euler's phi function) φ(n).
coprimes n = filter ((==1).(gcd n)) [2..n-1]


-- A decimal representation written with a repeating final 0 is said to 
-- terminate before these zeros. Instead of "1.585000..." one simply writes 
-- "1.585".[1] The decimal is also called a terminating decimal. Terminating 
-- decimals represent rational numbers of the form k/(2n5m).
isTerminatingDecimal n d = filter (not.(`elem` [2,5])) (primeFactors d') == []
  where
    [n',d'] = map (`div` (gcd n d)) [n,d]


-- Repetend for non-terminating decimal.
repetend n = loop (head lst) lst []
  where
  lst = map ((`quotRem` n).(*10)) [1..(n-1)]
  loop (q,r) ls rs
    | r `elem` rs = []
    | otherwise   = q:loop (lst!!(r-1)) lst (r:rs)


-- Coin Change problem denomTree is not used. Find a proper algorithm for that.
data Coin = Coin Int [Coin] deriving Show

denomTree [d]    = Coin d []
denomTree (d:ds) = Coin d (map denomTree (summablesFor d ds))
  where 
    summablesFor _ [] = error "Cannot compute summables"
    summablesFor 0 _  = []
    summablesFor n xxs@(x:xs)
      | x <= n    = xxs:summablesFor (n-x) xxs
      | otherwise = summablesFor n xs


-- Lazy, fast and generic algorithm for coin-change-problem
coinChanges = foldl fun ([[]] : repeat [])
  where
    fun acc coin = newacc
      where
       (poor, rich) = splitAt coin acc
       newacc = poor ++ zipWith (++) (map (map (coin:)) newacc) rich
