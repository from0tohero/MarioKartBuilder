import System.Environment (getArgs)

c_NUM_COLS :: Int
c_NUM_COLS = 12

splitOn :: Char -> String -> [String]
splitOn delim str = snd $ foldr f ([], []) (delim : str)
                      where 
                        f c (cur, acc) = if c == delim then ([], cur : acc)
                                                       else (c : cur, acc)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile(args !! 0)
    let 
        lines = splitOn '\n' content
        separ = fmap (splitOn '\t') lines 
        ll = filter (\l -> length l >= c_NUM_COLS) separ
    putStrLn $ show ll
