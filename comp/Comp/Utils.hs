module Comp.Utils
( maxInt, isPalindrome, spellNum, dayOf, wordsWhen
)
where

maxInt     = toInteger (maxBound :: Int)
firsts ls  = map fst ls
seconds ls = map snd ls

isPalindrome x = let str = show x in str == (reverse str)

spellNum num = case length $ show num of
                 1 -> unit $ show num
                 2 -> tens $ show num
                 3 -> hundreds $ show num
                 4 -> thousands $ show num
  where
    -- spell unit digit
    unit "0" = "zero"
    unit "1" = "one"
    unit "2" = "two"
    unit "3" = "three"
    unit "4" = "four"
    unit "5" = "five"
    unit "6" = "six"
    unit "7" = "seven"
    unit "8" = "eight"
    unit "9" = "nine"
    -- spell tens digit
    tens "10"     = "ten"
    tens "20"     = "twenty"
    tens "30"     = "thirty"
    tens "40"     = "forty"
    tens "50"     = "fifty"
    tens "80"     = "eighty"
    tens [x,'0']  = (unit [x]) ++ "ty"
    tens "11"     = "eleven"
    tens "12"     = "twelve"
    tens "13"     = "thirteen"
    tens "15"     = "fifteen"
    tens "18"     = "eighteen"
    tens ('1':xs) = (unit xs) ++ "teen"
    tens ['2',x]  = (tens "20") ++ " " ++ (unit [x])
    tens ['3',x]  = (tens "30") ++ " " ++ (unit [x])
    tens ['4',x]  = (tens "40") ++ " " ++ (unit [x])
    tens ['5',x]  = (tens "50") ++ " " ++ (unit [x])
    tens ['6',x]  = (tens "60") ++ " " ++ (unit [x])
    tens ['7',x]  = (tens "70") ++ " " ++ (unit [x])
    tens ['8',x]  = (tens "80") ++ " " ++ (unit [x])
    tens ['9',x]  = (tens "90") ++ " " ++ (unit [x])
    -- spell hundreds digit
    hundreds [x,'0','0'] = (unit [x]) ++ " " ++ "hundred"
    hundreds [x,'0', y ] = (unit [x]) ++ " " ++ "hundred" ++ " and " ++ (unit [y])
    hundreds [x, y,  z ] = (unit [x]) ++ " " ++ "hundred" ++ " and " ++ (tens [y,z])
    -- spell thousands digit
    thousands [x,'0','0','0'] = (unit [x]) ++ " " ++ "thousand"


isLeapYear mm yyyy 
  | mm == "feb" = check $ map ((==0).(rem yyyy)) [4, 100, 400]
  | mm == "jan" = check $ map ((==0).(rem yyyy)) [4, 100, 400]
  | otherwise = False
  where 
    check [True, False, _   ] = True
    check [True, True,  True] = True
    check [_,    _,     _   ] = False


-- To compute the (Gregorian calendar's) weekday for any date
dayOf dd mm yyyy = dayCode val
  where
    val = (dd + (monthCode mm) + yearPart + centPart - leapY) `rem` 7
    (century, year) = yyyy `quotRem` 100
    yearPart = year + (year `div` 4)
    centPart = (2*(century `rem` 4)) 
    leapY = if isLeapYear mm yyyy then 1 else 0
    monthCode "aug" = 0
    monthCode "feb" = 1
    monthCode "mar" = 1
    monthCode "nov" = 1
    monthCode "jun" = 2
    monthCode "sep" = 3
    monthCode "dec" = 3
    monthCode "apr" = 4
    monthCode "jul" = 4
    monthCode "jan" = 5
    monthCode "oct" = 5
    monthCode "may" = 6
    dayCode 0 = "Mon"
    dayCode 1 = "Tue"
    dayCode 2 = "Wed"
    dayCode 3 = "Thu"
    dayCode 4 = "Fri"
    dayCode 5 = "Sat"
    dayCode 6 = "Sun"


-- Split words using predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                   "" -> []
                   s' -> w : wordsWhen p s''
                     where (w, s'') = break p s'


-- Binary to Integer
bin2int bitstr = sum [ w*d | (w,d) <- zip weights (map bit bitstr) ]
  where
    bit '1' = 1
    bit '0' = 0
    weights = iterate (*2) 1

simpleInt r n p = p*n*r / 100             -- Simple Interest
compoundInt r n p = p * ((1 + r/100)**n)  -- Compound Interest

