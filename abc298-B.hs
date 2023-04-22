import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import Data.List

main :: IO ()
main = do
    [n] <- getIntList
    as <- replicateM n getIntList
    bs <- replicateM n getIntList
    putStrLn $ if solve as bs then "Yes" else "No"
    where
        solve as bs = check1 || check2 || check3 ||check4
            where
                a1 = as
                a2 = transpose $ reverse as
                a3 = reverse $ map reverse as
                a4 = transpose $ map reverse as
                check1 = a1 == zipWith (zipWith (*)) a1 bs
                check2 = a2 == zipWith (zipWith (*)) a2 bs
                check3 = a3 == zipWith (zipWith (*)) a3 bs
                check4 = a4 == zipWith (zipWith (*)) a4 bs

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
