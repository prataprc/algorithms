myhead (x:xs) = x

mytail (x:xs) = xs

myreverse []  = []
myreverse xs  = reverse' xs [] 
  where reverse' [] res     = res
        reverse' (x:xs) res = reverse' xs (x:res)

mysum []      = 0
mysum (x:xs)  = x + (mysum xs)

mylast [x,y]  = y
mylast (x:xs) = mylast xs

butLast = myhead . mytail . myreverse

elementAt (x:xs) 1   = x
elementAt xs     at  = elementAt xs (at-1)

length xs = mysum $ map (\x -> 1) xs

ispalindrom xs = xs == (myreverse xs)

flatten (x:xs) = x ++ (flatten xs)

compress []           = []
compress xs@(x:[])    = xs
compress (x:ys@(y:_)) = if x==y then compress ys else x:(compress ys)

pack []     = []
pack (x:xs) = pack' xs [x] x
  where pack' (x:xs) l y
          | x == y    = pack' xs (x:l) x
          | otherwise = l:pack' xs [x] x

rleEnc []      = []
rleEnc (x:xs)  = encode xs (1,x)
  where encode (x:xs) (m,y)
          | y == x    = encode xs (m+1,y)
          | otherwise = (m,y):encode xs (1,x)


rleDec []          = []
rleDec ((m,x):xs)  = (replicate m x) ++ rleDec xs
