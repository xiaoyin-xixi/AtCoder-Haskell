main :: IO ()
main = do
    [n, t] <- map (read :: String -> Integer) . words <$> getLine
    as <- map (read :: String -> Integer) . words <$> getLine
    let total = sum as
        rs = takeWhile (>0) $ scanl (\remain a -> remain - a) (t `mod` total) as
        (c, r) = (length rs, last rs)
    putStrLn $ show c ++ " " ++ show r