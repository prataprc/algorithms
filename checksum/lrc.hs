import Data.Bits
import System( getArgs )

lrc :: String -> Int -> Int
lrc [] acc = acc
lrc (x:xs) acc = lrc xs (xor acc (fromEnum x))

main = do
    args <- getArgs
    content <- readFile (head args)
    print (lrc content 0::Int)
