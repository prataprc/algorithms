import System (getArgs)
import Data.List (
    foldl', sortBy, sort, (\\), transpose, nub, intercalate, zip4, delete,
    group )
import Data.Char
import Data.Ratio
 
import Comp.Num
import Comp.Utils
import Comp.List

-- Add all the natural numbers below one thousand that are multiples of 3 or 5.
one n = (sum [3,6..n]) + (sum $ filter (\x -> x`rem`3/=0) [5,10..n])


-- By considering the terms in the Fibonacci sequence whose values do not 
-- exceed four million, find the sum of the even-valued terms.
two = sum (filter even $ takeWhile (<4000000) fibonacci)


-- What is the largest prime factor of the number 600851475143 ?
three = maximum $ primeFactors 600851475143


-- Find the largest palindrome made from the product of two 3-digit numbers ?
four = head $ filter fn $ filter isPalindrome $ filter (not.isPrime) [998008,997997..10000]
  where fn x = any (\(q,r) -> r==0 && q<1000) $ map (quotRem x) [990,979..770]


-- What is the smallest positive number that is evenly divisible by all of the 
-- numbers from 1 to 20? (Least common multiple between 1..20 ?)
five = product $ foldl (\acc fs -> acc ++ (fs \\ acc)) [] $ map primeFactors [2..20]


-- Find the difference between the sum of the squares of the first 100 natural numbers
-- and the square of the sum.
six  = abs $ (sumOfSquares 100) - (square $ sumOfN 100)


-- What is the 10001st prime number?
seven = primes !! 10001 


-- Find the greatest product of five consecutive digits in the 1000-digit number.
eight txt = slide 0 $ map digitToInt $ (concat.words) txt
  where 
    slide maxn (a:xs@(b:c:d:e:ys)) = slide (max maxn $ product [a,b,c,d,e]) xs
    slide maxn xs                  = max maxn $ product xs


-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc.
nine = product $ head [ [a,b,1000-a-b] | a <- [4..], b <- [a..400], isTriplet a b ]
  where isTriplet a b = (square (1000-a-b)) == ((square a)+(square b))


-- Find the sum of all the primes below two million.
ten = sum $ map toInteger $ doprimes 2000000

-- What is the greatest product of four adjacent numbers in any direction 
-- (up, down, left, right, or diagonally) in the 2020 grid?
one1 txt = sweep diag' $ sweep diag $ sweep (transpose array11) $ sweep array11 0
  where
    array11 = (map ((map read).words) $ lines txt) :: [[Int]]
    -- Lists of diagonals from top left
    diag  = map reverse $ array11Diag array11 $ replicate 19 []
    -- Lists of diagonals from top right
    diag' = map reverse $ array11Diag (map reverse array11) $ replicate 19 []
    array11Diag []          ll = ll
    array11Diag ((x:xs):ys) ll = array11Diag ys $ [x]:zip' xs ll
    zip' (x:xs) (ls:ll) = (x:ls):zip' xs ll
    zip' []     ll      = ll
    -- Slide [Int] list to compute largest product
    slide (a:b:c:d:xs) maxn = slide xs $ max maxn (a*b*c*d)
    slide xs           maxn = maxn
    -- Sweep the entire [[Int]] to compute largest product
    sweep []     maxn = maxn
    sweep (y:ys) maxn = sweep ys $ slide y maxn


-- What is the value of the first triangle number to have over five hundred
-- divisors ?
one2 = head $ dropWhile ((<500).numOfFactors) triangleNums
-- Gotcha : Why is the following Int version is slower than Integer.
-- one2 = head $ dropWhile ((<500).numOfFactors') triangleNums'


-- Work out the first ten digits of the sum of the following one-hundred 
-- 50-digit numbers.
one3 txt = take 10 $ show $ ( (sum.(map read).words) txt :: Integer)


-- Which starting number, under one million, produces the longest chain 
-- in collatz sequence ?
one4 = foldl maxChain (0,0) [2..1000000]
  where maxChain (l,n) x 
          | l <= collatzLen = (collatzLen, x)
          | otherwise      = (l,n)
          where collatzLen = length $ collatzChain x


-- How many routes are there through a 2020 grid ?
one5 = gridPath 20


-- What is the sum of the digits of the number 2^1000?
one6 = sum $ (map digitToInt $ show (2^1000) :: [Int])


-- If all the numbers from 1 to 1000 (one thousand) inclusive were 
-- written out in words, how many letters would be used?
one7 = sum $ map length $ words $ intercalate " " $ map spellNum [1..1000]


-- Find the maximum total from top to bottom of the triangle in one3.txt
one8 txt = head $ foldr foldTree (replicate 15 0) triangle
  where
    triangle = map ((map read).words) $ lines txt :: [[Int]]
    foldLevel [x]            = [x]
    foldLevel [x,y]          = [max x y]
    foldLevel (x:yys@(y:ys)) = max x y:foldLevel yys
    foldTree a y = foldLevel $ zipWith (+) a y


-- How many Sundays fell on the first of the month during the twentieth 
-- century (1 Jan 1901 to 31 Dec 2000) ?
one9 = length $ filter (=="Sun") [ dayOf 1 mm yyyy | mm <- mrange, yyyy <- yrange ]
  where 
    yrange = [1901..2000]
    mrange = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]


-- Find the sum of the digits in the number 100!
two0 = sum $ map digitToInt $ show $ factorial 100 :: Int


-- Evaluate the sum of all the amicable numbers under 10000.
two1 = sum $ amicables 10000


-- What is the total of all the name scores in the file?
two2 txt = sum $ zipWith (*) (map (sum.alphaVal.read) $ sort $ wordsWhen (==',') txt) [1..5163]
  where 
    alphaVal :: String -> [Int]
    alphaVal str = map ((subtract 64).ord) str


-- Find the sum of all the positive integers which cannot be written as the 
-- sum of two abundant numbers.
two3 = sum $ filter (\n-> calc n (n `div` 2) abundantNumbers []) [1..28123]
  where
    calc n till aas@(a:as) xs
      | a <= till  = calc n till as ((n-a):xs)
      | otherwise  = if xs == [] then check aas xs else check (n-(head xs):aas) xs
    check aas@(a:as) xxs@(x:xs)
      | a == x = False
      | a < x  = check as xxs
      | a > x  = check aas xs
    check _ _  = True


-- What is the millionth lexicographic permutation of the digits 
-- 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
two4 = concat $ map show $ nthPermutation 999999 10 [0..9]


-- What is the first term in the Fibonacci sequence to contain 1000 digits ?
two5 = head $ dropWhile fn $ zip [1..] $ map (length.show) $ tail fibonacci
  where fn (x,y) = y < 1000


-- Find the value of d  1000 for which 1/d contains the longest recurring cycle in 
-- its decimal fraction part.
two6 = maximum $ take 10 $ map (\x->(length $ repetend x, x)) $ filter isPrime' [1000,999..]


-- Find the product of the coefficients, a and b, for the quadratic 
-- expression that produces the maximum number of primes for consecutive values 
-- of n, starting with n = 0.
-- http://blog.mycila.com/2009/05/project-euler-problem-27.html
two7 = let (len,a,b) = maximum $ map primesN $ concat coeffs in (a*b,len,a,b)
  where 
    coeffs = [ [(a,b), (-a,b), (a,-b), (-a,-b)] | a <- [1..1000], b <- (takeWhile (<1000) primes') ]
    primesN (a,b) = (len,a,b)
      where len = length $ takeWhile (isPrime'.(poly a b)) [0..]
    poly a b n = n*n + a*n + b


-- What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral 
-- formed in the same way ?
two8 = sum $ takeWhile (<=max) seq
  where
    seq = 1: zipWith (+) (concat $ map ((replicate 4).(*2)) [1..]) seq
    max = 1001 * 1001


-- How many distinct terms are in the sequence generated by ab for 
-- 2<=a<=100 and 2<=b<=100 ?
two9 = length $ nub $ sort [ a^b | a <- [2..100], b <- [2..100] ]


-- Find the sum of all the numbers that can be written as the sum of fifth powers 
-- of their digits.
three0 = sum $ map value $ filter check $ possib
  where
    possib   = [ [a,b,c,d,e,f] | a <- [0,1], b <- [0..9], c <- [0..9],
                                 d <- [0..9], e <- [0..9], f <- [0..9] ]
    check ls = (value ls) > 1 && (value ls == sumOf ls)
    value ls = sum $ zipWith (*) (map (10^) [5,4..0]) ls
    sumOf ls = sum $ map (^5) ls


-- How many different ways can £2 be made using any number of coins ?
--   with coins,
--   1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p)
three1 = length $ coinChanges [1,2,5,10,20,50,100,200] !! 200
      

-- Find the sum of all numbers that can be written as pandigital products
-- between 1..9
three2 :: Int
three2 = sum $ map head $ group $ sort $ filter (/=0) $ map check $ permutationsR 9 [1..9]
  where
    check ls@[a,b,c,x,y,m,n,o,p] 
      | (a*100 + b*10 + c) * (x*10 + y) == (prod m n o p) = prod m n o p
      | otherwise                                         = check1 ls
      where prod m n o p = (m*1000 + n*100 + o*10 + p) 
    check1 [x,a,b,c,d,m,n,o,p]
      | (a*1000 + b*100 + c*10 + d) * x == (prod m n o p) = prod m n o p
      | otherwise                                         = 0
      where prod m n o p = (m*1000 + n*100 + o*10 + p) 


-- If the product of these four fractions is given in its lowest common terms,
-- find the value of the denominator.
three3 = denominator $ product $ filter (/=0%1) $ map fun permutalist
  where 
    fun [[a,b],[x,y]] 
      | b == x && (a%y == ratio a b x y) = ratio a b x y
      | otherwise                        = 0%1
      where ratio a b x y = (10*a + b) % (x*10 + y)
    permutalist = permutationsR 2 $ permutationsR 2 [1..9]


-- Find the sum of all numbers which are equal to the sum of the factorial of 
-- their digits.
three4 =  10



docmd []         = do mapM_ (docmd.(replicate 1).show) [1..34]
docmd ("1":[])   = do print $ "Problem 1 answer : "  ++ (show $ one 999)
docmd ("1":n:xs) = do print $ "Problem 1 answer : "  ++ (show $ one ((read n) -1))
docmd ("2":[])   = do print $ "Problem 2 answer : "  ++ (show two)
docmd ("3":[])   = do print $ "Problem 3 answer : "  ++ (show three)
docmd ("4":[])   = do print $ "Problem 4 answer : "  ++ (show four)
docmd ("5":[])   = do print $ "Problem 5 answer : "  ++ (show five)
docmd ("6":[])   = do print $ "Problem 6 answer : "  ++ (show six)
docmd ("7":[])   = do print $ "Problem 7 answer : "  ++ (show seven)
docmd ("8":[])   = do txt <- readFile "eight.txt"
                      print $ "Problem 8 answer : "  ++ (show $ eight txt)
docmd ("9":[])   = do print $ "Problem 9 answer : "  ++ (show nine)
docmd ("10":[])  = do print $ "Problem 10 answer : " ++ (show ten)
docmd ("11":[])  = do txt <- readFile "one1.txt"
                      print $ "Problem 11 answer : " ++ (show $ one1 txt)
docmd ("12":[])  = do print $ "Problem 12 answer : " ++ (show one2)
docmd ("13":[])  = do txt <- readFile "one3.txt"
                      print $ "Problem 13 answer : " ++ (show $ one3 txt)
docmd ("14":[])  = do print $ "Problem 14 answer : " ++ (show one4)
docmd ("15":[])  = do print $ "Problem 15 answer : " ++ (show one5)
docmd ("16":[])  = do print $ "Problem 16 answer : " ++ (show one6)
docmd ("17":[])  = do print $ "Problem 17 answer : " ++ (show one7)
docmd ("18":[])  = do txt <- readFile "one8.txt"
                      print $ "Problem 18 answer : " ++ (show $ one8 txt)
docmd ("19":[])  = do print $ "Problem 19 answer : " ++ (show one9)
docmd ("20":[])  = do print $ "Problem 20 answer : " ++ (show two0)
docmd ("21":[])  = do print $ "Problem 21 answer : " ++ (show two1)
docmd ("22":[])  = do txt <- readFile "two2.txt"
                      print $ "Problem 22 answer : " ++ (show $ two2 txt)
docmd ("23":[])  = do print $ "Problem 23 answer : " ++ (show two3)
docmd ("24":[])  = do print $ "Problem 24 answer : " ++ (show two4)
docmd ("25":[])  = do print $ "Problem 25 answer : " ++ (show two5)
docmd ("26":[])  = do print $ "Problem 26 answer : " ++ (show two6)
docmd ("27":[])  = do print $ "Problem 27 answer : " ++ (show two7)
docmd ("28":[])  = do print $ "Problem 28 answer : " ++ (show two8)
docmd ("29":[])  = do print $ "Problem 29 answer : " ++ (show two9)
docmd ("30":[])  = do print $ "Problem 30 answer : " ++ (show three0)
docmd ("31":[])  = do print $ "Problem 31 answer : " ++ (show three1)
docmd ("32":[])  = do print $ "Problem 32 answer : " ++ (show three2)
docmd ("33":[])  = do print $ "Problem 33 answer : " ++ (show three3)
docmd ("34":[])  = do print $ "Problem 34 answer : " ++ (show three4)

main = do
    args <- getArgs
    docmd args
