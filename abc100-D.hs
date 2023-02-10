import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

main :: IO ()
main = do
    [n,m] <- getIntList
    as <- replicateM n getIntList
    print $ maximum $ solve as m [(a,b,c)|a<-[-1,1],b<-[-1,1],c<-[-1,1]]
    where
        solve as m = map (\t -> let s = sum' t as in (0:dp s (replicate (length as) 0)) !! m)
        sum' (a,b,c) = map (\[x,y,z] -> a*x+b*y+c*z)
        dp as ts = let v = dp' $ zipWith (+) as ts in head v : dp (tail as) (tail $ reverse v)
            where
                dp' = foldl (\l x -> let a = max x (head l) in a `seq` a:l) [-inf]
                inf = 10000000000001

readInt = fst . fromJust . BS.readInt
readIntList = map readInt . BS.words
getIntList = readIntList <$> BS.getLine