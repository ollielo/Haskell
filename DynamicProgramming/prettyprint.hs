main :: IO ()

slack :: Int -> [String] -> Int
slack llimit ws = (llimit - (length $ unwords ws))^2

main =
  do
    contents <- readFile "poem.txt"
    let ls = lines contents
        ws = words contents
    mapM_ (putStrLn . show . slack 50 . words) ls
    