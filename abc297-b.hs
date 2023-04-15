import Data.List

main :: IO ()
main = do
    s <- getLine
    putStrLn $ if solve s then "Yes" else "No"
    where
        solve s = valid x y && (x'<z) && (z<y')
            where
                [x,y] = elemIndices 'B' s
                [x',y'] = elemIndices 'R' s
                valid x y = odd $ x+y
                [z] = elemIndices 'K' s
