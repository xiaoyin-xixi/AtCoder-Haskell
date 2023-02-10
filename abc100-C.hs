import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [n] <- getIntList
    as <- getIntList
    print $ sum $ map lenOfDiv2 as
    where
        lenOfDiv2 n = fromMaybe 0 $ findIndex odd $ iterate (`div` 2) n

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
