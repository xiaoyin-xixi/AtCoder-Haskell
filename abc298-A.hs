import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [n] <- getIntList
    s <- getLine
    putStrLn $ if solve s then "Yes" else "No"
    where
        solve s = ryo > 0 && fuka == 0
            where
                ryo = length $ filter (=='o') s
                fuka = length $ filter (=='x') s

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
