import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    n <- getInt
    as <- getIntList
    print $ solve as

solve :: [Int]  -> Int
solve as = fst $ foldl1 dp $ zip h1s h2s
    where
        dp (a1, a2) (h1, h2) = (min (a1 + h1) (a2 + h2), a1)
        h1s = zipWith (\a b -> abs(a-b)) (tail as) as
        h2s = 0:zipWith (\a b -> abs(a-b)) ((tail.tail) as) as

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
