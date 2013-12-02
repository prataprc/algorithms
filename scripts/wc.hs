import System (getArgs)
import Data.List (last)

main = do
   args <- getArgs
   txt <- readFile $ last args
   let ls = lines txt
       ws = concat $ map words ls
       count_l = length ls
       count_w = length ws
       count_c = length txt
   putStrLn $ (show count_l) ++ " " ++ (show count_w) ++ " " ++ (show count_c)
