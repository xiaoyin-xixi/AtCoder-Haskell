import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [n] <- getIntList
    print $ solve n
    where
        solve n
            | even n = n
            | otherwise = 2*n

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine