main :: IO ()
main = do
    n <- readLn
    mapM_ print $ reverse [0..n]