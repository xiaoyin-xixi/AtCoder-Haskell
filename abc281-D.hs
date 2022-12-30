import Data.List
import Control.Monad.State
import qualified Data.Map.Strict as M
 
type Memo k b = State (M.Map k b) b
 
memoize :: Ord k => (a -> Memo k b) -> (a -> k) -> a -> Memo k b
memoize f c x = do
    table <- get
    let key = c x
    case (M.lookup key table) of
        Just v -> return v
        Nothing -> do
            fx <- f x
            table' <- get
            put (M.insert key fx table')
            return fx
 
runM :: (a -> Memo k b) -> a -> b
runM m v = evalState (m v) M.empty
 
main :: IO ()
main = do
    [n, k, d] <- map (read :: String -> Int) . words <$> getLine
    as <- map (read :: String -> Int) . words <$> getLine
    print $ solve as k d
    where
        solve xs k d = runM memoDp (xs, k, 0)
            where
                memoDp :: ([Int], Int, Int) -> Memo (Int, Int, Int) Int
                memoDp (_, 0, r) = if r == 0 then return 0 else return (-1)
                memoDp ([], _, _) = return (-1)
                memoDp t = memoize memoDp' (\(xs, k, r) -> (length xs, k, r)) t
                    where
                        memoDp' (xxs@(x:xs), k, r) = do
                            a <- memoDp (xs, k, r)
                            b <- memoDp (xs, (k-1), ((r-x) `mod'` d))
                            if length xxs < k then return (-1) else
                                case (a, b) of
                                    (-1, -1) -> return (-1)
                                    (_, -1) -> return a
                                    (-1, _) -> return (b + x)
                                    (_, _) -> return $ max a (b + x)
                mod' a b = let r' = (a `mod` b) in if r' < 0 then r' + b else r'
