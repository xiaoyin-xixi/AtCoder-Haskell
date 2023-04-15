import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

main :: IO ()
main = do
    [n,d] <- getIntList
    ts <- getIntList
    print $ solve ts d
    where
        solve ts d = case i of
                        Just n -> ts !! (n+1)
                        _ -> -1
            where
                i = findIndex (<=d) diff
                diff = tail $ zipWith (-) ts (0:ts)

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
