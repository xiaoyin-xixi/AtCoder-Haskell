{-# LANGUAGE BangPatterns #-}
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [a,b] <- getIntegerList
    print $ solve a b
    where
        solve a b = loop a b 0 - 1
            where
                loop !a !b !cnt
                    | a==0 || b==0 = cnt
                    | a > b = let (m,q) = divMod a b in loop q b (cnt+m)
                    | a < b = let (m,q) = divMod b a in loop a q (cnt+m)
                    | otherwise = cnt+1

readInteger = fst . fromJust . BS.readInteger
readIntegerList = map readInteger . BS.words
getIntegerList = readIntegerList <$> BS.getLine
