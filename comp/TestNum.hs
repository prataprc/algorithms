import System (getArgs)

import Comp.Num
import Comp.Utils

data Result = ResInt    Int
            | ResStr    String
            | ResBool   Bool
            | ResIntg   Integer
            | RListInt  [Int]
            | RListIntg [Integer] deriving Show

docmd ("factorial":n:xs)= ResIntg res
    where res = factorial (read n)

docmd ("fib-nth":n:xs)  = ResIntg res
    where res = fibonacci !! (read n)

docmd ("fib-seq":n:xs)  = RListIntg res
    where res = take (read n) fibonacci

docmd ("fib-slice":x:y:xs) = RListIntg res
    where res = fibslice (read x) (read y)

docmd ("gcd":x:y:xs)    = ResIntg res
    where res = gcd (read x) (read y)

docmd ("gcd-b":x:y:xs)  = ResIntg res
    where res = binaryGCD (read x) (read y)

docmd ("factors":n:xs)  = RListIntg res
    where res = factors (read n)

docmd ("isprime":n:xs)
    | m < maxInt = ResBool (isPrime' m')
    | otherwise  = ResBool (isPrime m)
    where m  = read n
          m' = read n

docmd ("pfactors":n:xs)
    | m < maxInt = RListInt  (primeFactors' m')
    | otherwise  = RListIntg (primeFactors m)
    where m  = read n
          m' = read n

docmd ("numfactors":n:xs) 
    | m < maxInt = ResInt  (numOfFactors' m')
    | otherwise  = ResIntg (numOfFactors m)
    where m  = read n
          m' = read n

docmd ("primes":n:xs)
    | m < maxInt = RListInt  (takeWhile (<m') primes')
    | otherwise  = RListIntg (takeWhile (<m) primes)
    where m  = read n
          m' = read n

docmd ("doprimes":n:xs)  = RListInt (doprimes $ read n)

docmd xs                = ResStr "Invalid Arguments"

main = do
    args <- getArgs
    print (docmd args)
