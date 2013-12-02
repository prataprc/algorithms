module Num 
( shiftCipher,
)
where


-- Also known as caeser cipher, used to encode and decode string.
--    simply replaced each letter in the string by the letter three places 
--    further down in the alphabet, wrapping around at the end of the alphabet.
shiftCipher (c:str) = 
  where (q,r) = (ord c + 3) `quotRem` 
    cipher c = ord c + 3
