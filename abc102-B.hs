import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [n] <- getIntList
    as <- getIntList
    print $ solve as
    where
        solve as = maximum as - minimum as

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine