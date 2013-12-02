-- Insert an element `x` inside the list `ys` in sorted fashion.
insert x []     = [x]
insert x (y:ys) 
  | x <= y    = x:y:ys
  | otherwise = y:insert x ys

-- insertion sort
isort []     = []
isort (x:xs) = insert x $ isort xs

-- merge sort
merge []         ys         = ys
merge xs         []         = xs
merge xxs@(x:xs) yys@(y:ys)
  | x <= y    = x:msort xs  yys
  | otherwise = y:msort xxs ys

msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
  where 
    (ys, zs) = splitAt (length xs `div` 2) xs
