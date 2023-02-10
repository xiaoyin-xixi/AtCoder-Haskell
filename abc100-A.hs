import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [a, b] <- getIntList
    let dif = abs(a-b)
    if (16-(a+b-dif)) - 2*dif >= 0 then print "Yay!" else print ":("

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine
