import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [d, n] <- getIntList
    print $ [x*100^d | x <- [0..], x/=100] !! n

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
